type token =
  | Lparen
  | Rparen
  | Atom of string

let is_space (c : char) : bool =
  match c with
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let tokens_of_string (s : string) : token list =
  let rec go (out : token list) (i : int) : token list =
    if i >= String.length s
    then List.rev out
    else if is_space s.[i]
    then go out (i + 1)
    else if s.[i] = '('
    then go (Lparen :: out) (i + 1)
    else if s.[i] = ')'
    then go (Rparen :: out) (i + 1)
    else go' out i (i + 1)
  and go' (out : token list) (i : int) (j : int) : token list =
    if j >= String.length s
       || is_space s.[j]
       || s.[j] = '('
       || s.[j] = ')'
    then go (Atom (String.sub s i (j - i)) :: out) j
    else go' out i (j + 1)
  in go [] 0

type sexpr =
  | Atom of string
  | List of sexpr list

let rec sexpr_of_tokens (_toks : token list) : (sexpr * token list) option =
  match _toks with
  | [] -> None
  | Atom s :: rest ->
      Some (Atom s, rest)
  | Lparen :: rest ->
      let (es, rest') = sexprs_of_tokens rest in
      begin match rest' with
      | Rparen :: rest'' -> Some (List es, rest'')
      | _ -> None
      end
  | Rparen :: _ -> None

and sexprs_of_tokens (_toks : token list) : sexpr list * token list =
  match sexpr_of_tokens _toks with
  | None -> ([], _toks)
  | Some (e, rest) ->
      let (es, rest') = sexprs_of_tokens rest in
      (e :: es, rest')

let parse_sexpr (_s : string) : sexpr option =
  match sexpr_of_tokens (tokens_of_string _s) with
  | Some (e, []) -> Some e
  | _ -> None

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec expr_of_sexpr (_sexpr : sexpr) : expr option =
  match _sexpr with
  | Atom s ->
      begin match int_of_string_opt s with
      | Some n -> Some (Int n)
      | None -> None
      end
  | List [Atom op; a; b] ->
      begin match expr_of_sexpr a, expr_of_sexpr b with
      | Some ea, Some eb ->
          begin match op with
          | "+" -> Some (Add (ea, eb))
          | "-" -> Some (Sub (ea, eb))
          | "*" -> Some (Mul (ea, eb))
          | "/" -> Some (Div (ea, eb))
          | _ -> None
          end
      | _ -> None
      end
  | _ -> None

let parse (_s : string) : expr option =
  match parse_sexpr _s with
  | Some sx -> expr_of_sexpr sx
  | None -> None

let rec eval (_e : expr) : int =
  match _e with
  | Int n -> n
  | Add (a, b) -> eval a + eval b
  | Sub (a, b) -> eval a - eval b
  | Mul (a, b) -> eval a * eval b
  | Div (a, b) -> eval a / eval b

let interp (_s : string) : int option =
  match parse _s with
  | None -> None
  | Some e ->
      try Some (eval e)
      with Division_by_zero -> None
