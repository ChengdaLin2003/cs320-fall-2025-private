include Utils

let parse (s : string) : prog option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

type constr = ty * ty
type solution = (string * ty) list

let unify (_cs : constr list) : solution option =
  assert false (* TODO *)

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
