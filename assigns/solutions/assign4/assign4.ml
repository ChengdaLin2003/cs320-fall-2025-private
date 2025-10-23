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

let rec sexpr_of_tokens (toks : token list) : (sexpr * token list) option =
  match toks with
  | Atom s :: toks -> Some (Atom s, toks)
  | Lparen :: toks -> (
    match sexprs_of_tokens toks with
    | es, Rparen :: toks -> Some (List es, toks)
    | _ -> None
  )
  | _ -> None
and sexprs_of_tokens (toks : token list) : sexpr list * token list =
  match sexpr_of_tokens toks with
  | Some (e, toks) ->
    let es, toks = sexprs_of_tokens toks in
    e :: es, toks
  | None -> [], toks

let parse_sexpr (s : string) : sexpr option =
  match s |> tokens_of_string |> sexpr_of_tokens with
  | Some (e, []) -> Some e
  | _ -> None

type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let expr_of_sexpr (sexpr : sexpr) : expr option =
  let rec go sexpr =
    match sexpr with
    | Atom s -> (
        match int_of_string_opt s with
        | Some n -> Some (Int n)
        | None -> None
      )
    | List [Atom "+"; e1; e2] -> (
        match go e1, go e2 with
        | Some e1, Some e2 -> Some (Add (e1, e2))
        | _ -> None
      )
    | List [Atom "-"; e1; e2] -> (
        match go e1, go e2 with
        | Some e1, Some e2 -> Some (Sub (e1, e2))
        | _ -> None
      )
    | List [Atom "*"; e1; e2] -> (
        match go e1, go e2 with
        | Some e1, Some e2 -> Some (Mul (e1, e2))
        | _ -> None
      )
    | List [Atom "/"; e1; e2] -> (
        match go e1, go e2 with
        | Some e1, Some e2 -> Some (Div (e1, e2))
        | _ -> None
      )
    | _ -> None
  in go sexpr

let parse (s : string) : expr option =
  match parse_sexpr s with
  | Some sexpr -> expr_of_sexpr sexpr
  | None -> None

let rec eval (e : expr) : int =
  match e with
  | Int n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2

let interp (s : string) : int option =
  match parse s with
  | Some e -> Some (eval e)
  | None -> None
