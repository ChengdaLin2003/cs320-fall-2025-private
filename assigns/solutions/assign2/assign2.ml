
let drop_leading (n : int) (l : int list) : int list =
  let rec go l =
    match l with
    | [] -> []
    | x :: xs ->
      if x = n
      then go xs
      else l
  in go l

let drop_trailing (n : int) (l : int list) : int list =
  List.rev (drop_leading n (List.rev l))

let split_on_char (c : char) (s : string) : string list =
  let rec go acc i j =
    if i + j >= String.length s
    then List.rev (String.sub s i j :: acc)
    else
      if String.get s (i + j) = c
      then go (String.sub s i j :: acc) (i + j + 1) 0
      else go acc i (j + 1)
  in go [] 0 0

let parse_fractran (input : string) : Q.t list =
  let lines = split_on_char ' ' input in
  if lines = [""]
  then []
  else
    let rec program acc lines =
      match lines with
      | [] -> List.rev acc
      | rat :: lines ->
         let q =
           match split_on_char '/' rat with
           | n :: d :: [] -> Q.make (Z.of_string n) (Z.of_string d)
           | _ -> assert false
         in program (q :: acc) lines
    in
    program [] lines

let eval_fractran (program : Q.t list) (input : Z.t) : Z.t =
  let rec go p n =
    match p with
    | [] -> n
    | q :: qs ->
      let next = Q.mul (Q.of_bigint n) q in
      if Q.den next = Z.one
      then go program (Q.num next)
      else go qs n
  in
  go program input

let interp_fractran (input : string) : Z.t -> Z.t =
  eval_fractran (parse_fractran input)

let max_fractran (i : int) (j : int) : int =
  let program = "5/6 5/2 5/3" in
  let input = Z.((~$2 ** i) * (~$3 ** j)) in
  let output = interp_fractran program input in
  let div_by_five n =
    let rec go acc n =
      if n = Z.one
      then acc
      else go (acc + 1) Z.(n / ~$5)
    in go 0 n
  in div_by_five output

let fib_fractran (n : int) : int =
  let program = "17/65 133/34 17/19 23/17 2233/69 23/29 31/23 74/341 31/37 41/31 129/287 41/43 13/41 1/13 1/3" in
  let input = Z.(~$78 * (~$5 ** n)) in
  let output = interp_fractran program input in
  let div_by_two n =
    let rec go acc n =
      if n = Z.one
      then acc
      else go (acc + 1) Z.(n / ~$2)
    in go 0 n
  in div_by_two output
