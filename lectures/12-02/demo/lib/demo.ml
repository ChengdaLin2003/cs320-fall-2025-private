include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

type constr = ty * ty
type solution = (string * ty) list

let free_vars =
  let open VarSet in
  let rec go = function
    | TInt | TBool -> empty
    | TVar a -> singleton a
    | TFun (t1, t2) -> union (go t1) (go t2)
  in go

let ty_subst t a =
  let rec go = function
  | TInt -> TInt
  | TBool -> TBool
  | TFun (t1, t2) -> TFun (go t1, go t2)
  | TVar b -> if a = b then t else TVar b
  in go

let unify (cs : constr list) : solution option =
  let rec unify s = function
    | [] -> Some (List.rev s)
    | c :: cs ->
      match c with
      | t1, t2 when t1 = t2 -> unify s cs
      | TFun (s1, t1), TFun (s2, t2) ->
        unify s ((s1, s2) :: (t1, t2) :: cs)
      | TVar a, t when not (VarSet.mem a (free_vars t)) ->
        unify
          ((a, t) :: s)
          (List.map
             (fun (t1, t2) ->
                ty_subst t a t1,
                ty_subst t a t2
             )
             cs)
      | t, TVar a -> unify s ((TVar a, t) :: cs)
      | _ -> None
  in
  unify [] cs

let is_well_typed (_p : prog) : bool =
  true (* TODO *)

let rec eval (env : dyn_env) (e : expr) : value =
  let rec go e =
    let go_int e = match go e with | VNum n -> n | _ -> assert false in
    let go_bool e = match go e with | VBool b -> b | _ -> assert false in
    match e with
    | Let(x, e1, e2) -> eval (Env.add x (go e1) env) e2
    | App (e1, e2) -> (
        match go e1 with
        | VClos (x, e, env') -> eval (Env.add x (go e2) env') e
        | _ -> assert false
      )
    | Var x -> Env.find x env
    | Num n -> VNum n
    | Fun (x, e) -> VClos (x, e, env)
    | Add (e1, e2) -> VNum (go_int e1 + go_int e2)
    | Eq (e1, e2) -> VBool (go_int e1 = go_int e2)
    | If (e1, e2, e3) -> if go_bool e1 then go e2 else go e3
  in go e

let rec desugar = function
  | [] -> assert false
  | (name, binding) :: [] -> Let (name, binding, Var name)
  | (name, binding) :: p ->
    Let (name, binding, desugar p)

let interp (s : string) : value option =
  match parse s with
  | Some p ->
    if is_well_typed p
    then Some (eval Env.empty (desugar p))
    else None
  | None -> None
