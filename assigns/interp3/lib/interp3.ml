include Utils

let parse (s : string) : prog option =
  try
    let lexbuf = Lexing.from_string s in
    Some (Parser.prog Lexer.read lexbuf)
  with _ -> None

type subst = ty Env.t

let empty_subst : subst = Env.empty

let lookup_subst (x : ident) (s : subst) : ty option =
  try Some (Env.find x s) with Not_found -> None

let rec apply_subst_ty (s : subst) (t : ty) : ty =
  match t with
  | TUnit | TInt | TFloat | TBool -> t
  | TVar a ->
      begin match lookup_subst a s with
      | Some t' -> apply_subst_ty s t'
      | None -> t
      end
  | TList t1 -> TList (apply_subst_ty s t1)
  | TOption t1 -> TOption (apply_subst_ty s t1)
  | TPair (t1, t2) ->
      TPair (apply_subst_ty s t1, apply_subst_ty s t2)
  | TFun (t1, t2) ->
      TFun (apply_subst_ty s t1, apply_subst_ty s t2)

let rec free_ty_vars (t : ty) : VarSet.t =
  match t with
  | TUnit | TInt | TFloat | TBool -> VarSet.empty
  | TVar a -> VarSet.singleton a
  | TList t1
  | TOption t1 -> free_ty_vars t1
  | TPair (t1, t2)
  | TFun (t1, t2) ->
      VarSet.union (free_ty_vars t1) (free_ty_vars t2)

let occurs (a : ident) (t : ty) : bool =
  VarSet.mem a (free_ty_vars t)

let rec unify (s : subst) (cs : constr list) : subst option =
  match cs with
  | [] -> Some s
  | (t1, t2) :: rest ->
      let t1 = apply_subst_ty s t1 in
      let t2 = apply_subst_ty s t2 in
      match t1, t2 with
      | TUnit, TUnit
      | TInt, TInt
      | TFloat, TFloat
      | TBool, TBool ->
          unify s rest

      | TList a, TList b
      | TOption a, TOption b ->
          unify s ((a, b) :: rest)

      | TPair (a1, a2), TPair (b1, b2)
      | TFun (a1, a2),  TFun (b1, b2) ->
          unify s ((a1, b1) :: (a2, b2) :: rest)

      | TVar x, t
      | t, TVar x ->
          if t = TVar x then
            unify s rest
          else if occurs x t then
            None
          else
            let t = apply_subst_ty s t in
            let s' = Env.add x t s in
            unify s' rest
      | _, _ -> None


let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  match unify empty_subst cs with
  | None -> None
  | Some s ->
      let ty' = apply_subst_ty s ty in
      let vars = free_ty_vars ty' in
      Some (Forall (vars, ty'))


let type_of (_ctxt : stc_env) (_e : expr) : ty_scheme option =
  assert false

let is_well_typed (_p : prog) : bool =
  assert false

exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_expr (_env : dyn_env) (_e : expr) : value =
  assert false

let eval (p : prog) : value =
  let rec nest = function
    | [] -> Unit
    | [{ is_rec; name; binding }] ->
        Let { is_rec; name; binding; body = Var name }
    | { is_rec; name; binding } :: rest ->
        Let { is_rec; name; binding; body = nest rest }
  in
  let e = nest p in
  eval_expr Env.empty e

let interp (input : string) : (value, error) result =
  match parse input with
  | Some prog ->
      if is_well_typed prog then
        Ok (eval prog)
      else
        Error TypeError
  | None -> Error ParseError