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

let sexpr_of_tokens (_toks : token list) : (sexpr * token list) option =
  assert false
and sexprs_of_tokens (_toks : token list) : sexpr list * token list =
  assert false

let parse_sexpr (_s : string) : sexpr option =
  assert false

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let expr_of_sexpr (_sexpr : sexpr) : expr option =
  assert false

let parse (_s : string) : expr option =
  assert false

let eval (_e : expr) : int =
  assert false

let interp (_s : string) : int option =
  assert false
