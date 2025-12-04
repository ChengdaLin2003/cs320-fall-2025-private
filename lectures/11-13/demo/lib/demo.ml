include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let rec desugar (p : prog) : expr =
  match p with
  | [] -> Unit
  | [TopLet (x, ty, e)] -> Let (x, ty, e, Var x)
  | [TopLetRec (f, x, arg_ty, out_ty, e)] ->
    LetRec (f, x, arg_ty, out_ty, e, Var f)
  | TopLet (x, ty, e) :: lets ->
    Let (x, ty, e, desugar lets)
  | TopLetRec (f, x, arg_ty, out_ty, e) :: lets ->
    LetRec (f, x, arg_ty, out_ty, e, desugar lets)

let rec type_of (ctxt: ctxt) (e : expr) : ty option =
  match e with
  | Unit -> Some UnitTy
  | Var x -> Env.find_opt x ctxt
  | Num _ -> Some IntTy
  | Fun (x, t1, e) -> (
    match type_of (Env.add x t1 ctxt) e with
    | Some t2 -> Some (FunTy (t1, t2))
    | None -> None
  )
  | Add (e1, e2) -> (
      match type_of ctxt e1 with
      | Some IntTy -> (
          match type_of ctxt e2 with
          | Some IntTy -> Some IntTy
          | _ -> None
        )
      | _ -> None
    )
  | Eq (e1, e2) -> (
      match type_of ctxt e1 with
      | Some IntTy -> (
          match type_of ctxt e2 with
          | Some IntTy -> Some BoolTy
          | _ -> None
        )
      | _ -> None
    )
  | If (e1, e2, e3) -> (
      match type_of ctxt e1 with
      | Some BoolTy -> (
          match type_of ctxt e2 with
          | Some t2 -> (
              match type_of ctxt e3 with
              | Some t3 ->
                if t2 = t3
                then Some t2
                else None
              | None -> None
            )
          | None -> None
        )
      | _ -> None
    )
  | App (e1, e2) -> (
    match type_of ctxt e1 with
      | Some FunTy (t1, t2) -> (
          match type_of ctxt e2 with
          | Some t ->
            if t = t1
            then Some t2
            else None
          | None -> None
        )
      | _ -> None
  )
  | Let (x, ty, e1, e2) -> (
      match type_of ctxt e1 with
      | Some t1 ->
        if t1 = ty
        then type_of (Env.add x ty ctxt) e2
        else None
      | None -> None
    )
  | LetRec (f, x, arg_ty, out_ty, e1, e2) -> (
      let new_ctxt =
        ctxt
        |> Env.add f (FunTy (arg_ty, out_ty))
        |> Env.add x arg_ty
      in
      match type_of new_ctxt e1 with
      | Some t1 ->
        if t1 = out_ty
        then type_of (Env.add f (FunTy (arg_ty, out_ty)) ctxt) e2
        else None
      | None -> None
    )

let type_of e = type_of Env.empty e

let rec eval (env : env) (e : expr) : value =
  let int = function
    | VNum n -> n
    | _ -> assert false
  in
  let bool = function
    | VBool b -> b
    | _ -> assert false
  in
  match e with
  | Unit -> VUnit
  | Var x -> Env.find x env
  | Num n -> VNum n
  | Fun (x, _, e) -> VClos (None, env, x, e)
  | Add (e1, e2) -> VNum (int (eval env e1) + int (eval env e2))
  | Eq (e1, e2) -> VBool (int (eval env e1) = int (eval env e2))
  | If (e1, e2, e3) ->
    if bool (eval env e1)
    then eval env e2
    else eval env e3
  | App (e1, e2) -> (
      match eval env e1 with
      | VClos (None, old_env, x, e) ->
        eval (insert (x, eval env e2) old_env) e
      | VClos (Some name, old_env, x, e) ->
        eval
          (old_env
           |> insert (name, VClos (Some name, old_env, x, e))
           |> insert (x, eval env e2))
          e
      | _ -> assert false
    )
  | Let (x, _, e1, e2) ->
    eval (insert (x, (eval env e1)) env) e2
  | LetRec (f, x, _, _, e1, e2) ->
    eval (insert (f, VClos (Some f, env, x, e1)) env) e2

let eval e = eval empty e

let interp (s : string) : value option =
  let _ = print_endline "parsing..." in
  match parse s with
  | Some prog -> (
      let _ = print_endline "desugaring..." in
      let expr = desugar prog in
      let _ = print_endline "type checking..." in
      match type_of expr with
      | Some _ ->
        let _ = print_endline "evaluating..." in
        Some (eval expr)
      | None -> None
    )
  | None -> None
