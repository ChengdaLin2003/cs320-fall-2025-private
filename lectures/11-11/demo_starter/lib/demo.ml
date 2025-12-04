include Utils

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let rec desugar (p : prog) : expr =
  match p with
  | [] -> Unit
  | Let (x, e) :: lets ->
    Let (x, e, desugar lets)
  | LetRec (f, x, e) :: lets -> assert false


let rec eval (env : env) (e : expr) : value option =
  match e with
  | Var x -> lookup x env
  | Num n -> Some (VNum n)
  | Fun (x, e) -> Some (VClos (None, env, x, e))
  | Add (e1, e2) -> (
      match eval env e1, eval env e2 with
      | Some (VNum m), Some (VNum n) -> Some (VNum (m + n))
      | _ -> None
    )
  | Eq (e1, e2) -> (
      match eval env e1, eval env e2 with
      | Some (VNum m), Some (VNum n) -> Some (VBool (m = n))
      | _ -> None
    )
  | If (e1, e2, e3) -> (
      match eval env e1 with
      | Some (VBool b) ->
        if b
        then eval env e2
        else eval env e3
      | _ -> None
    )
  | App (e1, e2) -> (
      match eval env e1 with
      | Some (VClos (None, old_env, x, e)) -> (
          match eval env e2 with
          | Some v2 ->
            let new_env = insert (x, v2) old_env in
            eval new_env e
          | _ -> None
        )
      | Some (VClos (Some name, old_env, x, e)) -> (
          match eval env e2 with
          | Some v2 ->
            let new_env =
              old_env
              |> insert (name, VClos (Some name, old_env, x, e))
              |> insert (x, v2)
            in
            eval new_env e
          | _ -> None
        )
      | _ -> None
    )
  | Let (x, e1, e2) -> (
      match eval env e1 with
      | Some v1 -> eval (insert (x, v1) env) e2
      | _ -> None
    )
  | LetRec (f, x, e1, e2) ->
    eval (insert (f, VClos (Some f, env, x, e1)) env) e2

let eval e = eval empty e

let interp (s : string) : value option =
  match parse s with
  | Some expr -> eval expr
  | None -> None
