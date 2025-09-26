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

let load (_l : int list) : registers =
  let rec func i l =
    match l with
    | [] -> []
    | x :: xs ->
      let rest = func (i + 1) xs in
        if x = 0 then rest
        else (i, x) :: rest
  in
  func 0 _l

let lookup (_rs : registers) (_i : int) : int = 
  let rec func rs i =
    match rs with
    | [] -> 0
    | (key, value) :: xs ->
      if key = i then value
      else func xs i
  in
  func _rs _i

let incr (_rs : registers) (_i : int) : registers =
  let rec func rs i =
    match rs with
    | [] -> [i, 1]
    | (key, value) :: xs ->
      if key = i then
        let newv = value + 1 in
        if newv = 0 then xs
        else (key, newv) :: xs
      else if (key > i) then (i, 1) :: (key, value) :: xs
      else (key, value) :: func xs i
  in
  func _rs _i

let zero (_rs : registers) (_i : int) : registers =
  let rec func rs i =
    match rs with
    | [] -> []
    | (key, value) :: xs ->
      if key = i then xs
      else (key, value) :: func xs i
  in
  func _rs _i

let transfer (_rs : registers) (_i : int) (_j : int) : registers =
  if _i = _j then _rs
  else
    let i_value = lookup _rs _i in
      let rec func rs j =
      match rs with
      | [] -> if i_value = 0 then [] else [(j, i_value)]
      | (key, value) :: xs ->
        if key = j then
          if i_value = 0 then xs else (key, i_value) :: xs
        else if key > j then
          if i_value = 0 then (key, value) :: xs else (j, i_value) :: (key, value) :: xs
        else (key, value) :: func xs j
    in
    func _rs _j

let parse_urm (_tokens : string list) : int list list =
  let to_int = int_of_string in
  let rec go acc toks =
    match toks with
    | [] -> List.rev acc
    | "Z" :: si :: rest ->
        go ([0; to_int si] :: acc) rest
    | "I" :: si :: rest ->
        go ([1; to_int si] :: acc) rest
    | "T" :: si :: sj :: rest ->
        go ([2; to_int si; to_int sj] :: acc) rest
    | "J" :: si :: sj :: sk :: rest ->
        go ([3; to_int si; to_int sj; to_int sk] :: acc) rest
    | _ -> assert false
  in
  go [] _tokens

let eval_urm (_prog : int list list) (_rs : registers) : registers =
  let len = List.length _prog in
  let rec loop (rs : registers) (pc : int) : registers =
    if pc < 0 || pc >= len then rs
    else
      match List.nth _prog pc with
      | [0; i] ->
          loop (zero rs i) (pc + 1)
      | [1; i] ->
          loop (incr rs i) (pc + 1)
      | [2; i; j] ->
          loop (transfer rs i j) (pc + 1)
      | [3; i; j; k] ->
          if lookup rs i = lookup rs j
          then loop rs k
          else loop rs (pc + 1)
      | _ ->
          assert false
  in
  loop _rs 0
let interp_urm (prog : string) (args : int list) : int =
  prog
  |> sep_on_whitespace
  |> parse_urm
  |> fun p -> eval_urm p (load args)
  |> fun rs -> lookup rs 0

(* Challenge problem: make this work for negative inputs *)
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
