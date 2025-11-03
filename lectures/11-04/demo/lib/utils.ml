type expr =
  | Int of int
  | Var of string
  | Fun of string * expr
  | App of expr * expr

type prog = expr

type value =
  | IntV of int
  | FunV of string * expr
