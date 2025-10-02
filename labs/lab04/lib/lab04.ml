type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

let convert (l : int_or_string list) : int_list_or_string_list list =
  let rec go l =
    match l with
    | [] -> []
    | Int n :: xs ->
      (match go xs with
       | IntList ys :: rest -> IntList (n :: ys) :: rest
       | l -> IntList [n] :: l)
    | String s :: xs ->
      (match go xs with
       | StringList ys :: rest -> StringList (s :: ys) :: rest
       | l -> StringList [s] :: l)
  in go l
