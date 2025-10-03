
let rec rev_append (l : 'a list) (r : 'a list) : 'a list =
  match l with
  | [] -> r
  | x :: xs -> rev_append xs (x :: r)

let is_whitespace c = List.mem c [' '; '\n'; '\t'; '\r']

let split_on_whitespace (s : string) : string list =
  let rec go acc i j =
    if i + j >= String.length s
    then List.rev (String.sub s i j :: acc)
    else
      if is_whitespace (String.get s (i + j))
      then go (String.sub s i j :: acc) (i + j + 1) 0
      else go acc i (j + 1)
  in go [] 0 0

let sep_on_whitespace (s : string) : string list =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | h :: t ->
      if h = ""
      then go acc t
      else go (h :: acc) t
  in go [] (split_on_whitespace s)

type registers = (int * int) list

let load (l : int list) : registers =
  let rec go i acc l =
    match l with
    | [] -> List.rev acc
    | x :: xs ->
      if x = 0
      then go (i + 1) acc xs
      else go (i + 1) ((i, x) :: acc) xs
  in go 0 [] l

let lookup (rs : registers) (i : int) : int =
  let rec go rs =
    match rs with
    | [] -> 0
    | (j, n) :: rs ->
      if j = i
      then n
      else go rs
  in go rs

let set (rs : registers) (i : int) (n : int) : registers =
  let rec go acc rs =
    match rs with
    | [] ->
      List.rev ((if n = 0 then [] else [(i, n)]) @ acc)
    | (j, m) :: t ->
      if j = i
      then rev_append acc ((if n = 0 then [] else [(i, n)]) @ t)
      else if i < j
      then rev_append acc ((if n = 0 then [] else [(i, n)]) @ rs)
      else go ((j, m) :: acc) t
  in go [] rs

let incr (rs : registers) (i : int) : registers = set rs i (lookup rs i + 1)
let zero (rs : registers) (i : int) : registers = set rs i 0
let transfer (rs : registers) (i : int) (j : int) : registers = set rs j (lookup rs i)

let parse_urm (prog : string list) : int list list =
  let rec go acc l =
    match l with
    | [] -> List.rev acc
    | "Z" :: i :: t -> go ([0; int_of_string i] :: acc)  t
    | "I" :: i :: t -> go ([1; int_of_string i] :: acc) t
    | "T" :: i :: j :: t ->
      let inst = [2; int_of_string i; int_of_string j] in
      go (inst :: acc) t
    | "J" :: i :: j :: k :: t ->
      let inst =
        [
          3;
          int_of_string i;
          int_of_string j;
          int_of_string k;
        ]
      in go (inst :: acc) t
    | _ -> assert false
  in go [] prog

let eval_urm (prog : int list list) (rs : registers) : registers =
  let rec go counter rs =
    if counter >= List.length prog
    then rs
    else
      match List.nth prog counter with
      | [0;i] -> go (counter + 1) (zero rs i)
      | [1;i] -> go (counter + 1) (incr rs i)
      | [2;i;j] -> go (counter + 1) (transfer rs i j)
      | [3;i;j;k] ->
        if lookup rs i = lookup rs j
        then go k rs
        else go (counter + 1) rs
      | _ -> assert false
  in go 0 rs

let interp_urm (prog : string) (args : int list) : int =
  prog
  |> sep_on_whitespace
  |> parse_urm
  |> fun prog -> eval_urm prog (load args)
  |> fun rs -> lookup rs 0

(* Challenge problem: make this work for negative values *)
let max_urm (i : int) (j : int) : int =
  interp_urm
    "
    T 0 2
    Z 0
    J 1 3 100
    I 0
    I 3
    J 0 0 2
    "
    [i; j]


let fibonacci_urm (i : int) : int =
  interp_urm
    "
    I 2
    J 0 5 11
      T 2 3
      J 1 4 7
        I 2 I 4
        J 0 0 3
      T 3 1
      Z 4 I 5
      J 0 0 1
    T 1 0
    "
    [i]
