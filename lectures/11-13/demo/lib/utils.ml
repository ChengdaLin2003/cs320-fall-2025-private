type ty =
  | UnitTy
  | IntTy
  | BoolTy
  | FunTy of ty * ty

type expr =
  | Unit
  | Var of string
  | Num of int
  | Fun of string * ty * expr
  | Add of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of string * ty * expr * expr
  | LetRec of string * string * ty * ty * expr * expr

type toplet =
  | TopLet of string * ty * expr
  | TopLetRec of string * string * ty * ty * expr

type prog = toplet list

module Env = Map.Make(String)

type value =
  | VUnit
  | VBool of bool
  | VNum of int
  | VClos of string option * env * string * expr


(* Bindings from strings to values *)
and env = value Env.t

(* empty environment *)
let empty : env = Env.empty

(* look up in environment *)
let lookup (x : string) (e : env) : value option = Env.find_opt x e

(* insert new binding in environment *)
let insert ((x, v) : string * value) (e : env) : env = Env.add x v e

type ctxt = ty Env.t
