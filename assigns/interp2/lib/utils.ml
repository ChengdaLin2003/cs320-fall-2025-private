type bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

type expr =
  | Unit
  | True | False
  | Num of int
  | Var of string
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Bop of bop * expr * expr

type value =
  | VUnit
  | VBool of bool
  | VNum of int
  | VFun of string * expr

type error =
  | UnknownVar of string
  | InvalidArgs of bop
  | InvalidIfCond
  | InvalidApp
  | DivByZero
  | ParseFail

let string_of_bop = function
  | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/" | Mod -> "mod"
  | Lt -> "<" | Lte -> "<=" | Gt -> ">" | Gte -> ">="
  | Eq -> "=" | Neq -> "<>"
  | And -> "&&" | Or -> "||"

let rec string_of_expr = function
  | Unit -> "()"
  | True -> "true"
  | False -> "false"
  | Num n -> string_of_int n
  | Var x -> x
  | Let (x,e1,e2) ->
      "let " ^ x ^ " = " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2
  | If (c,t,f) ->
      "if " ^ string_of_expr c ^ " then " ^ string_of_expr t ^ " else " ^ string_of_expr f
  | Fun (x,e) -> "(fun " ^ x ^ " -> " ^ string_of_expr e ^ ")"
  | App (e1,e2) -> "(" ^ string_of_expr e1 ^ " " ^ string_of_expr e2 ^ ")"
  | Bop (op,e1,e2) ->
      "(" ^ string_of_expr e1 ^ " " ^ string_of_bop op ^ " " ^ string_of_expr e2 ^ ")"

let string_of_value = function
  | VUnit -> "()"
  | VBool b -> string_of_bool b
  | VNum n -> string_of_int n
  | VFun _ -> "<fun>"

let string_of_error = function
  | UnknownVar x     -> "unknown variable: " ^ x
  | InvalidArgs op   -> "invalid operands for operator " ^ string_of_bop op
  | InvalidIfCond    -> "if condition is not a boolean"
  | InvalidApp       -> "attempted to apply a non-function value"
  | DivByZero        -> "division by zero"
  | ParseFail        -> "parse failure"

type prog = expr