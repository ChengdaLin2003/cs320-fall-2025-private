
type expr =
  | True
  | False
  | Int of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lte of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr

let is_lowercase (c : char) : bool =
  c >= 'a' && c <= 'z'

let is_valid_var : string -> bool= String.for_all is_lowercase

let parse (s : string) : expr option = assert false

type ty =
  | IntTy
  | BoolTy

module M = Map.Make(String)
type ctxt = ty M.t

let empty_ctxt : ctxt = M.empty
let add_binding (x : string) (t : ty) (gamma : ctxt) : ctxt = M.add x t gamma
let check_binding (x : string) (gamma : ctxt) : ty option = M.find_opt x gamma

let type_of (gamma : ctxt) (e : expr) : ty option = assert false

let subst (e1 : expr) (x : string) (e2 : expr) : expr =
  let rec go e =
    match e with
    | True -> True
    | False -> False
    | Int n -> Int n
    | Var y -> if y = x then e1 else Var y
    | Add (e1, e2) -> Add (go e1, go e2)
    | Sub (e1, e2) -> Sub (go e1, go e2)
    | Mul (e1, e2) -> Mul (go e1, go e2)
    | Div (e1, e2) -> Div (go e1, go e2)
    | Lte (e1, e2) -> Lte (go e1, go e2)
    | If (e1, e2, e3) -> If (go e1, go e2, go e3)
    | Let (y, e1, e2) -> Let (y, go e1, if y = x then e2 else go e2)
  in go e2

type value =
  | IntV of int
  | BoolV of bool

let expr_of_value (v : value) : expr = assert false

let eval (e : expr) : value = assert false

let interp ?(print=true) (s : string) : value option =
  let print s = if print then print_endline s else () in
  let _ = print "parsing..." in
  match parse s with
  | None -> None
  | Some e ->
    let _ = print "type_checking..." in
    match type_of empty_ctxt e with
    | None -> None
    | Some _ ->
      let _ = print "evaluating..." in
      Some (eval e)
