(* -------------------- Binary Operators -------------------- *)
(** [bop] represents all binary operators supported in the language.
    They cover arithmetic, comparison, and boolean connectives. *)
type bop =
  | Add | Sub | Mul | Div | Mod              (* arithmetic: + - * / mod *)
  | Lt | Lte | Gt | Gte | Eq | Neq           (* comparison: < <= > >= = <> *)
  | And | Or                                 (* boolean logic *)

(* -------------------- Abstract Syntax Tree -------------------- *)
(** [expr] is the abstract syntax tree of expressions. *)
type expr =
  | Unit                                     (* () *)
  | True | False                             (* booleans *)
  | Num of int                               (* integer literal *)
  | Var of string                            (* variable reference *)
  | Let of string * expr * expr              (* let x = e1 in e2 *)
  | If of expr * expr * expr                 (* if c then t else f *)
  | Fun of string * expr                     (* fun x -> e *)
  | App of expr * expr                       (* e1 e2 *)
  | Bop of bop * expr * expr                 (* e1 op e2 *)

(* -------------------- Runtime Values -------------------- *)
(** [value] are runtime values produced by the evaluator. *)
type value =
  | VUnit
  | VBool of bool
  | VNum of int
  | VFun of string * expr    (* a simple closure with just parameter & body *)

(* -------------------- Errors -------------------- *)
(** [error] enumerates all runtime/driver errors we surface. *)
type error =
  | UnknownVar of string
  | InvalidArgs of bop
  | InvalidIfCond
  | InvalidApp
  | DivByZero
  | ParseFail

(* -------------------- Pretty-printers -------------------- *)

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

(* Pretty-print an interpreter error. *)
let string_of_error = function
  | UnknownVar x     -> "unknown variable: " ^ x
  | InvalidArgs op   -> "invalid operands for operator " ^ string_of_bop op
  | InvalidIfCond    -> "if condition is not a boolean"
  | InvalidApp       -> "attempted to apply a non-function value"
  | DivByZero        -> "division by zero"
  | ParseFail        -> "parse failure"

(* -------------------- Top-level program -------------------- *)
(** The driver expects [prog] to be the same as [expr] at this stage. *)
type prog = expr