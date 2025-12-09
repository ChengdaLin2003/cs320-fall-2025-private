include Utils

(*****************************************************************)
(* *)
(* Parser Wrapper                                                *)
(* *)
(*****************************************************************)

let parse (s : string) : prog option =
  try
    let lexbuf = Lexing.from_string s in
    Some (Parser.prog Lexer.read lexbuf)
  with _ -> None

(*****************************************************************)
(* *)
(* Type Inference                                                *)
(* *)
(*****************************************************************)

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
      if t1 = t2 then unify s rest else
      match t1, t2 with
      | TList a, TList b
      | TOption a, TOption b ->
          unify s ((a, b) :: rest)

      | TPair (a1, a2), TPair (b1, b2)
      | TFun (a1, a2),  TFun (b1, b2) ->
          unify s ((a1, b1) :: (a2, b2) :: rest)

      | TVar x, t
      | t, TVar x ->
          if occurs x t then
            None
          else
            (* Update substitution: x |-> t, and apply this to existing range *)
            let s_new_mapping = Env.singleton x t in
            let s_updated = Env.map (apply_subst_ty s_new_mapping) s in
            let final_s = Env.add x t s_updated in
            unify final_s rest
            
      | _, _ -> None

let principle_type (ty : ty) (cs : constr list) : ty_scheme option =
  match unify empty_subst cs with
  | None -> None
  | Some s ->
      let ty' = apply_subst_ty s ty in
      let vars = free_ty_vars ty' in
      Some (Forall (vars, ty'))

(* Helper: Instantiate a type scheme with fresh variables *)
let instantiate (Forall (vars, ty) : ty_scheme) : ty =
  let subst = 
    VarSet.fold 
      (fun v acc -> Env.add v (TVar (Utils.gensym ())) acc) 
      vars 
      Env.empty 
  in
  apply_subst_ty subst ty

(* Helper: Recursive Inference *)
let rec infer (env : stc_env) (e : expr) : ty * constr list =
  match e with
  | Unit -> (TUnit, [])
  | Int _ -> (TInt, [])
  | Float _ -> (TFloat, [])
  | Bool _ -> (TBool, [])
  | Nil -> (TList (TVar (Utils.gensym ())), [])
  | ENone -> (TOption (TVar (Utils.gensym ())), [])
  
  | Var x ->
      (match Env.find_opt x env with
       | Some sch -> (instantiate sch, [])
       | None -> failwith ("Unbound variable " ^ x))

  | Annot (e1, t_ann) ->
      let (t1, c1) = infer env e1 in
      (t_ann, (t1, t_ann) :: c1)

  | Assert e1 ->
      let (t1, c1) = infer env e1 in
      (TUnit, (t1, TBool) :: c1)

  | ESome e1 ->
      let (t1, c1) = infer env e1 in
      (TOption t1, c1)

  | If (e1, e2, e3) ->
      let (t1, c1) = infer env e1 in
      let (t2, c2) = infer env e2 in
      let (t3, c3) = infer env e3 in
      (t2, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)

  | Bop (op, e1, e2) ->
      let (t1, c1) = infer env e1 in
      let (t2, c2) = infer env e2 in
      let (res_ty, constraints) = 
        match op with
        | Add | Sub | Mul | Div | Mod -> (TInt, [(t1, TInt); (t2, TInt)])
        | AddF | SubF | MulF | DivF | PowF -> (TFloat, [(t1, TFloat); (t2, TFloat)])
        | Lt | Lte | Gt | Gte -> (TBool, [(t1, t2)]) 
        | Eq | Neq -> (TBool, [(t1, t2)])
        | And | Or -> (TBool, [(t1, TBool); (t2, TBool)])
        | Comma -> (TPair (t1, t2), [])
        | Cons -> (TList t1, [(t2, TList t1)])
      in
      (res_ty, constraints @ c1 @ c2)

  | Fun (x, topt, body) ->
      let arg_ty = match topt with Some t -> t | None -> TVar (Utils.gensym ()) in
      let env' = Env.add x (Forall (VarSet.empty, arg_ty)) env in
      let (tbody, cbody) = infer env' body in
      (TFun (arg_ty, tbody), cbody)

  | App (e1, e2) ->
      let (t1, c1) = infer env e1 in
      let (t2, c2) = infer env e2 in
      let ret_ty = TVar (Utils.gensym ()) in
      (ret_ty, (t1, TFun (t2, ret_ty)) :: c1 @ c2)

  | Let { is_rec; name; binding; body } ->
      if is_rec then
        let alpha = TVar (Utils.gensym ()) in
        let env_rec = Env.add name (Forall (VarSet.empty, alpha)) env in
        let (tbind, cbind) = infer env_rec binding in
        let (tbody, cbody) = infer env_rec body in
        (tbody, (alpha, tbind) :: cbind @ cbody)
      else
        let (tbind, cbind) = infer env binding in
        let env' = Env.add name (Forall (VarSet.empty, tbind)) env in
        let (tbody, cbody) = infer env' body in
        (tbody, cbind @ cbody)

  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      let (tm, cm) = infer env matched in
      let (tnil, cnil) = infer env nil_case in
      let alpha = TVar (Utils.gensym ()) in
      let env_cons = 
        Env.add hd_name (Forall (VarSet.empty, alpha)) 
          (Env.add tl_name (Forall (VarSet.empty, TList alpha)) env) 
      in
      let (tcons, ccons) = infer env_cons cons_case in
      (tnil, (tm, TList alpha) :: (tnil, tcons) :: cm @ cnil @ ccons)

  | OptMatch { matched; some_name; some_case; none_case } ->
      let (tm, cm) = infer env matched in
      let (tnone, cnone) = infer env none_case in
      let alpha = TVar (Utils.gensym ()) in
      let env_some = Env.add some_name (Forall (VarSet.empty, alpha)) env in
      let (tsome, csome) = infer env_some some_case in
      (tnone, (tm, TOption alpha) :: (tnone, tsome) :: cm @ cnone @ csome)

  | PairMatch { matched; fst_name; snd_name; case } ->
      let (tm, cm) = infer env matched in
      let t1 = TVar (Utils.gensym ()) in
      let t2 = TVar (Utils.gensym ()) in
      let env' = 
        Env.add fst_name (Forall (VarSet.empty, t1)) 
          (Env.add snd_name (Forall (VarSet.empty, t2)) env) 
      in
      let (tc, cc) = infer env' case in
      (tc, (tm, TPair (t1, t2)) :: cm @ cc)


let type_of (env : stc_env) (e : expr) : ty_scheme option =
  try
    let (ty, cs) = infer env e in
    principle_type ty cs
  with _ -> None

(* Checks if the top-level program is well-typed *)
let is_well_typed (p : prog) : bool =
  let rec go env prog =
    match prog with
    | [] -> true
    | { is_rec; name; binding } :: rest ->
        let ty_scheme_opt = 
          if is_rec then
             let alpha = TVar (Utils.gensym ()) in
             let env_rec = Env.add name (Forall (VarSet.empty, alpha)) env in
             match type_of env_rec binding with
             | Some sch -> Some sch
             | None -> None
          else
             type_of env binding
        in
        match ty_scheme_opt with
        | Some sch -> go (Env.add name sch env) rest
        | None -> false
  in
  go Env.empty p

(*****************************************************************)
(* *)
(* Evaluation                                                    *)
(* *)
(*****************************************************************)

exception AssertFail
exception DivByZero
exception CompareFunVals

let int_of_val = function VInt i -> i | _ -> failwith "Expected int"
let bool_of_val = function VBool b -> b | _ -> failwith "Expected bool"
let float_of_val = function VFloat f -> f | _ -> failwith "Expected float"

(* Helper to check for function values in polymorphic comparisons (Extra Credit) *)
let rec check_fun_val (v : value) : unit =
  match v with
  | VClos _ -> raise CompareFunVals
  | VPair (v1, v2) -> check_fun_val v1; check_fun_val v2
  | VList l -> List.iter check_fun_val l
  | VSome v1 -> check_fun_val v1
  | _ -> ()

let rec eval_expr (env : dyn_env) (e : expr) : value =
  match e with
  | Unit -> VUnit
  | Int n -> VInt n
  | Float f -> VFloat f
  | Bool b -> VBool b
  | Nil -> VList []
  | ENone -> VNone
  | Var x -> 
      (try Env.find x env 
       with Not_found -> failwith ("Runtime error: Unbound variable " ^ x))
  
  | Annot (e1, _) -> eval_expr env e1
  | Assert e1 -> 
      if bool_of_val (eval_expr env e1) then VUnit else raise AssertFail
  
  | ESome e1 -> VSome (eval_expr env e1)
  
  | If (cond, t, f) ->
      if bool_of_val (eval_expr env cond) then eval_expr env t else eval_expr env f
  
  | Fun (x, _, body) ->
      VClos { arg = x; body; env; name = None }
      
  | App (e1, e2) ->
      let v1 = eval_expr env e1 in
      let v2 = eval_expr env e2 in
      (match v1 with
       | VClos { arg; body; env = clos_env; name } ->
           let env_with_rec = 
             match name with
             | Some n -> Env.add n v1 clos_env
             | None -> clos_env
           in
           let env' = Env.add arg v2 env_with_rec in
           eval_expr env' body
       | _ -> failwith "Application of non-function")
       
  | Let { is_rec; name; binding; body } ->
      if is_rec then
        match binding with
        | Fun (arg, _, fun_body) ->
            let clos = VClos { arg; body = fun_body; env; name = Some name } in
            let env' = Env.add name clos env in
            eval_expr env' body
        | _ -> 
             let v = eval_expr env binding in
             let env' = Env.add name v env in
             eval_expr env' body
      else
        let v = eval_expr env binding in
        let env' = Env.add name v env in
        eval_expr env' body

  | Bop (op, e1, e2) ->
      (match op with
       | And -> 
           (match eval_expr env e1 with
            | VBool false -> VBool false
            | VBool true -> eval_expr env e2
            | _ -> failwith "And expects bool")
       | Or ->
           (match eval_expr env e1 with
            | VBool true -> VBool true
            | VBool false -> eval_expr env e2
            | _ -> failwith "Or expects bool")
       | _ ->
           let v1 = eval_expr env e1 in
           let v2 = eval_expr env e2 in
           match op with
           | Add -> VInt (int_of_val v1 + int_of_val v2)
           | Sub -> VInt (int_of_val v1 - int_of_val v2)
           | Mul -> VInt (int_of_val v1 * int_of_val v2)
           | Div -> 
               let d = int_of_val v2 in
               if d = 0 then raise DivByZero else VInt (int_of_val v1 / d)
           | Mod -> 
               let d = int_of_val v2 in
               if d = 0 then raise DivByZero else VInt (int_of_val v1 mod d)
           | AddF -> VFloat (float_of_val v1 +. float_of_val v2)
           | SubF -> VFloat (float_of_val v1 -. float_of_val v2)
           | MulF -> VFloat (float_of_val v1 *. float_of_val v2)
           | DivF -> VFloat (float_of_val v1 /. float_of_val v2)
           | PowF -> VFloat (float_of_val v1 ** float_of_val v2)
           | Lt -> 
               check_fun_val v1; check_fun_val v2;
               VBool (v1 < v2)
           | Lte -> 
               check_fun_val v1; check_fun_val v2;
               VBool (v1 <= v2)
           | Gt -> 
               check_fun_val v1; check_fun_val v2;
               VBool (v1 > v2)
           | Gte -> 
               check_fun_val v1; check_fun_val v2;
               VBool (v1 >= v2)
           | Eq -> 
               check_fun_val v1; check_fun_val v2;
               VBool (v1 = v2)
           | Neq -> 
               check_fun_val v1; check_fun_val v2;
               VBool (v1 <> v2)
           | Comma -> VPair (v1, v2)
           | Cons -> 
               (match v2 with
                | VList l -> VList (v1 :: l)
                | _ -> failwith "Cons expects a list on the right")
           | _ -> failwith "Impossible bop"
      )

  | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      (match eval_expr env matched with
       | VList [] -> eval_expr env nil_case
       | VList (h :: t) ->
           let env' = Env.add hd_name h (Env.add tl_name (VList t) env) in
           eval_expr env' cons_case
       | _ -> failwith "Match error: expected list")

  | OptMatch { matched; some_name; some_case; none_case } ->
      (match eval_expr env matched with
       | VNone -> eval_expr env none_case
       | VSome v ->
           let env' = Env.add some_name v env in
           eval_expr env' some_case
       | _ -> failwith "Match error: expected option")

  | PairMatch { matched; fst_name; snd_name; case } ->
      (match eval_expr env matched with
       | VPair (v1, v2) ->
           let env' = Env.add fst_name v1 (Env.add snd_name v2 env) in
           eval_expr env' case
       | _ -> failwith "Match error: expected pair")

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