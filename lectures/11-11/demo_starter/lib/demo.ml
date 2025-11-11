include Utils

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

let rec eval (_env : env) (_e : expr) : value option =
  assert false

let interp (s : string) : value option =
  match parse s with
  | Some expr -> eval Env.empty expr
  | None -> None
