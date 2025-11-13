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

let type_of (_e : expr) : ty option = Some UnitTy

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
