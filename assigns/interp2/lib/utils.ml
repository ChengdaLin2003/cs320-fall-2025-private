(* ---------- Binary operators ---------- *)

type bop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Lte | Gt | Gte | Eq | Neq
  | And | Or

(* ---------- Types ---------- *)

type ty =
  | TInt
  | TBool
  | TUnit
  | TFun of ty * ty

(* ---------- Core expressions (typed AST) ---------- *)

type expr =
  | Unit
  | True
  | False
  | Num of int
  | Var of string
  | Let    of string * ty * expr * expr  (* let x : t = e1 in e2 *)
  | LetRec of string * ty * expr * expr  (* let rec f : t = e1 in e2 *)
  | If of expr * expr * expr
  | Fun of string * ty * expr            (* fun (x:t) -> e *)
  | App of expr * expr
  | Bop of bop * expr * expr
  | Assert of expr

(* ---------- Surface toplevel declarations ---------- *)

type toplet = {
  is_rec : bool;               (* let 或 let rec *)
  name   : string;             (* 函数名 *)
  args   : (string * ty) list; (* 参数 (x1:t1) ... (xk:tk) *)
  ann    : ty;                 (* 返回类型注解 *)
  body   : expr;               (* 函数体 (expr) *)
}

type prog = toplet list

(* ---------- Runtime values, environments ---------- *)

type env  = (string * value) list
and value =
  | VUnit
  | VBool of bool
  | VNum  of int
  | VClos of env * string option * expr

type tenv = (string * ty) list

(* ---------- Static / dynamic errors ---------- *)

type error =
  | UnknownVar of string
  | IfTyErr of ty * ty
  | IfCondTyErr of ty
  | OpTyErrL of bop * ty * ty
  | OpTyErrR of bop * ty * ty
  | FunArgTyErr of ty * ty
  | FunAppTyErr of ty
  | LetTyErr of ty * ty
  | LetRecErr of string
  | AssertTyErr of ty
  | ParseFail

exception DivByZero
exception AssertFail

(* ---------- Pretty-printers (for main/tests) ---------- *)

let rec string_of_ty = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun (t1, t2) ->
      "(" ^ string_of_ty t1 ^ " -> " ^ string_of_ty t2 ^ ")"

let string_of_value = function
  | VUnit -> "()"
  | VBool b -> string_of_bool b
  | VNum n -> string_of_int n
  | VClos _ -> "<fun>"

let string_of_bop = function
  | Add -> "Add" | Sub -> "Sub" | Mul -> "Mul" | Div -> "Div" | Mod -> "Mod"
  | Lt -> "Lt" | Lte -> "Lte" | Gt -> "Gt" | Gte -> "Gte"
  | Eq -> "Eq" | Neq -> "Neq" | And -> "And" | Or -> "Or"

let string_of_error = function
  | UnknownVar x ->
      "UnknownVar(" ^ x ^ ")"
  | IfTyErr (t1,t2) ->
      "IfTyErr(" ^ string_of_ty t1 ^ "," ^ string_of_ty t2 ^ ")"
  | IfCondTyErr t ->
      "IfCondTyErr(" ^ string_of_ty t ^ ")"
  | OpTyErrL (op,t1,t2) ->
      "OpTyErrL(" ^ string_of_bop op ^ "," ^ string_of_ty t1 ^ "," ^ string_of_ty t2 ^ ")"
  | OpTyErrR (op,t1,t2) ->
      "OpTyErrR(" ^ string_of_bop op ^ "," ^ string_of_ty t1 ^ "," ^ string_of_ty t2 ^ ")"
  | FunArgTyErr (t1,t2) ->
      "FunArgTyErr(" ^ string_of_ty t1 ^ "," ^ string_of_ty t2 ^ ")"
  | FunAppTyErr t ->
      "FunAppTyErr(" ^ string_of_ty t ^ ")"
  | LetTyErr (t1,t2) ->
      "LetTyErr(" ^ string_of_ty t1 ^ "," ^ string_of_ty t2 ^ ")"
  | LetRecErr f ->
      "LetRecErr(" ^ f ^ ")"
  | AssertTyErr t ->
      "AssertTyErr(" ^ string_of_ty t ^ ")"
  | ParseFail ->
      "ParseFail"