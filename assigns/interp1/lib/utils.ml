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
  | If of expr * expr * expr                 (* if e1 then e2 else e3 *)
  | Fun of string * expr                     (* anonymous function fun x -> e *)
  | App of expr * expr                       (* function application e1 e2 *)
  | Bop of bop * expr * expr                 (* binary operation e1 op e2 *)

(* -------------------- Program Type -------------------- *)
(** In this language, a program is simply an expression. *)
type prog = expr

(* -------------------- Runtime Values -------------------- *)
(** [value] represents evaluated runtime results.
    Functions are represented as closures (here, just syntax-based). *)
type value =
  | VNum of int                              (* integer value *)
  | VBool of bool                            (* boolean value *)
  | VFun of string * expr                    (* function closure: parameter + body *)
  | VUnit                                    (* unit value *)

(* -------------------- Runtime Errors -------------------- *)
(** [error] enumerates all possible runtime and parsing errors. *)
type error =
  | UnknownVar of string                     (* variable not found in environment *)
  | InvalidArgs of bop                       (* wrong operand type for operator *)
  | InvalidIfCond                            (* non-boolean if condition *)
  | InvalidApp                               (* applying non-function *)
  | DivByZero                                (* division/mod by zero *)
  | ParseFail                                (* parser failed to produce AST *)

(* -------------------- Pretty-printing -------------------- *)
(** [string_of_value v] converts a runtime value to string.
    Used for debugging and printing final interpreter results. *)
let string_of_value = function
  | VNum n -> string_of_int n
  | VBool b -> string_of_bool b
  | VFun _ -> "<fun>"                        (* functions have no printable body *)
  | VUnit -> "()"