include Utils

let parse (s : string) : expr option =
  let lb = Lexing.from_string s in
  try Some (Parser.prog Lexer.read lb) with Parser.Error -> None

let subst (_v : value) (_x : string) (e : expr) : expr = e

let eval (_e : expr) : (value, error) result = Error ParseFail

let interp (s : string) : (value, error) result =
  match parse s with
  | Some _e -> Error ParseFail
  | None -> Error ParseFail