include Utils

(* ------------------- *)
(* Parsing entry point *)
(* ------------------- *)

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | prog -> Some prog
  | exception _ -> None

(* -------------------------------- *)
(* Substitutions and unification    *)
(* -------------------------------- *)

(* A substitution maps type variables (ident) to types. *)
type subst = (ident * ty) list

let empty_subst : subst = []

(* Apply a substitution to a type *)
let rec subst_ty (s : subst) (t : ty) : ty =
  match t with
  | TUnit | TInt | TFloat | TBool -> t
  | TVar x ->
      (match List.assoc_opt x s with
       | Some t' -> t'
       | None -> t)
  | TList t1      -> TList   (subst_ty s t1)
  | TOption t1    -> TOption (subst_ty s t1)
  | TPair (t1,t2) -> TPair   (subst_ty s t1, subst_ty s t2)
  | TFun  (t1,t2) -> TFun    (subst_ty s t1, subst_ty s t2)

let subst_constr (s : subst) ((t1,t2) : constr) : constr =
  (subst_ty s t1, subst_ty s t2)

let subst_constrs (s : subst) (cs : constr list) : constr list =
  List.map (subst_constr s) cs

(* Composition: apply s2 then s1 *)
let compose_subst (s2 : subst) (s1 : subst) : subst =
  let s1' = List.map (fun (x,t) -> (x, subst_ty s2 t)) s1 in
  let s2_only =
    List.filter (fun (x,_) -> not (List.exists (fun (y,_) -> x = y) s1)) s2
  in
  s1' @ s2_only

(* Occurs check: does variable x appear in type t? *)
let rec occurs (x : ident) (t : ty) : bool =
  match t with
  | TUnit | TInt | TFloat | TBool -> false
  | TVar y -> x = y
  | TList t1
  | TOption t1 -> occurs x t1
  | TPair (t1,t2)
  | TFun  (t1,t2) -> occurs x t1 || occurs x t2

(* Most general unifier of a set of constraints *)
let rec unify (cs : constr list) : subst option =
  match cs with
  | [] -> Some empty_subst
  | (t1,t2) :: rest ->
      match (t1,t2) with
      | t1, t2 when t1 = t2 ->
          unify rest

      | TVar a, t
      | t, TVar a ->
          if occurs a t then
            None
          else
            let s = [ (a, t) ] in
            (match unify (subst_constrs s rest) with
             | None -> None
             | Some s' -> Some (compose_subst s' s))

      | TFun (a1,a2), TFun (b1,b2) ->
          unify ((a1,b1) :: (a2,b2) :: rest)

      | TList a, TList b ->
          unify ((a,b) :: rest)

      | TOption a, TOption b ->
          unify ((a,b) :: rest)

      | TPair (a1,a2), TPair (b1,b2) ->
          unify ((a1,b1) :: (a2,b2) :: rest)

      | _ ->
          None

(* Free type variables of a type *)
let rec ftv_ty (t : ty) : VarSet.t =
  match t with
  | TUnit | TInt | TFloat | TBool -> VarSet.empty
  | TVar x -> VarSet.singleton x
  | TList t1
  | TOption t1 -> ftv_ty t1
  | TPair (t1,t2)
  | TFun  (t1,t2) ->
      VarSet.union (ftv_ty t1) (ftv_ty t2)

(* ----------------------------- *)
(* principle_type : main target  *)
(* ----------------------------- *)

(* Given a type τ and constraint set C, compute the principle
   type scheme ∀α1 ... αk. Sτ where S is the most general unifier of C,
   or return None if C is not unifiable. *)
let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  match unify cs with
  | None -> None
  | Some s ->
      let ty' = subst_ty s ty in
      let vars = ftv_ty ty' in
      Some (Forall (vars, ty'))

(* ----------------------------- *)
(* Stubs for later parts of MP3 *)
(* ----------------------------- *)

let type_of (_ctxt : stc_env) (_e : expr) : ty_scheme option =
  assert false

let is_well_typed (_p : prog) : bool =
  assert false

exception AssertFail
exception DivByZero
exception CompareFunVals

let eval_expr (_env : dyn_env) (_e : expr) : value =
  (* Not required for the one-week check-in. *)
  failwith "eval_expr is not implemented yet"

let eval (p : prog) : value =
  let rec nest = function
    | [] -> Unit
    | [ { is_rec; name; binding } ] ->
        Let { is_rec; name; binding; body = Var name }
    | { is_rec; name; binding } :: rest ->
        Let { is_rec; name; binding; body = nest rest }
  in
  let e = nest p in
  eval_expr Env.empty e

let interp (input : string) =
  match parse input with
  | Some prog ->
      if is_well_typed prog then
        Ok (eval prog)
      else
        Error TypeError
  | None -> Error ParseError