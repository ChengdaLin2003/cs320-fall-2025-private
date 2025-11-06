type expr =
  | Var of string
  | Num of int
  | Fun of string * expr
  | Add of expr * expr
  | Eq of expr * expr
  | If of expr * expr * expr
  | App of expr * expr
  | Let of string * expr * expr
  | LetRec of string * string * expr * expr

type prog = expr

module Env = Map.Make(String)

type value =
  | VBool of bool
  | VNum of int
  | VFun of string * expr

(* Bindings from strings to values *)
type env = value Env.t

(* empty environment *)
let empty : env = Env.empty

(* look up in environment *)
let lookup (x : string) (e : env) : value option = Env.find_opt x e

(* insert new binding in environment *)
let insert ((x, v) : string * value) (e : env) : env = Env.add x v e
