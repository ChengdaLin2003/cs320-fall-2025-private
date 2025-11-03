type bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

type expr =
  | Unit
  | True
  | False
  | Num of int
  | Var of string
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | App of expr * expr
  | Bop of bop * expr * expr

(* 按 spec：程序就是一个表达式 *)
type prog = expr

type value =
  | VNum of int
  | VBool of bool
  | VFun of string * expr
  | VUnit

type error =
  | UnknownVar of string
  | InvalidArgs of bop
  | InvalidIfCond
  | InvalidApp
  | DivByZero
  | ParseFail

let string_of_value = function
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VFun _ -> "<fun>"
  | VUnit -> "()"