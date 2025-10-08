let curry3 f =
  fun a ->
    fun b ->
      fun c ->
        f (a, b, c)

let uncurry3 f =
  fun (a, b, c) -> f a b c

let rec filter_map p l =
  match l with
  | [] -> []
  | x :: xs ->
      match p x with
      | None -> filter_map p xs
      | Some y -> y :: filter_map p xs

type 'a rtree = Node of 'a * 'a rtree list

let rec map_rtree f (Node (v, children)) =
  Node (f v, List.map (map_rtree f) children)

let rec filter_rtree f (Node (v, children)) =
  if f v then
    let kept_children = filter_map (filter_rtree f) children in
    Some (Node (v, kept_children))
  else
    None

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

let rec string_of_expr e =
  match e with
  | Int n -> string_of_int n
  | Var x -> x
  | Call (fname, args) ->
      let args_str = String.concat ", " (List.map string_of_expr args) in
      fname ^ "(" ^ args_str ^ ")"
  | Bop (op, e1, e2) ->
      "(" ^ string_of_expr e1 ^ " " ^ string_of_op op ^ " " ^ string_of_expr e2 ^ ")"

  
let split_on_char c s =
  let rec aux i j acc =
    if j = String.length s then
      List.rev (String.sub s i (j - i) :: acc)
    else if s.[j] = c then
      aux (j + 1) (j + 1) (String.sub s i (j - i) :: acc)
    else
      aux i (j + 1) acc
  in
  aux 0 0 []

let rec string_of_stmt s =
  match s with
  | Assign (x, e) ->
      x ^ " = " ^ string_of_expr e
  | Print e ->
      "print(" ^ string_of_expr e ^ ")"
  | Return e ->
      "return " ^ string_of_expr e
  | FunDef (name, args, body) ->
      let args_str = String.concat ", " args in
      let indent_all_lines s =
        split_on_char '\n' s |> List.map (fun line -> "    " ^ line) |> String.concat "\n"
      in
      let body_str =
        match body with
        | [] -> "    pass"
        | _ ->
            body
            |> List.map string_of_stmt
            |> List.map indent_all_lines
            |> String.concat "\n"
      in
      "def " ^ name ^ "(" ^ args_str ^ "):\n" ^ body_str

let string_of_prog (p : prog) =
  String.concat "\n" (List.map string_of_stmt p)
