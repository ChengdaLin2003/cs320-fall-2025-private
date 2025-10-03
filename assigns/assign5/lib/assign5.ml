
let curry3 = assert false

let uncurry3 = assert false

let filter_map _ _ = assert false

type 'a rtree = Node of 'a * 'a rtree list

let map_rtree _ _ = assert false

let filter_rtree _ _ = assert false

type op = Add | Sub | Mul | Div

type expr =
  | Int of int
  | Var of string
  | Call of string * expr list
  | Bop of op * expr * expr

type stmt =
  | FunDef of string * string list * stmt list
  | Assign of string * expr
  | Print of expr
  | Return of expr

type prog = stmt list

let string_of_op = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"

let string_of_expr _ = assert false

let string_of_stmt _ = assert false

let string_of_prog _ =  assert false
