
let rec drop_leading (_n : int) (_l : int list) : int list =
  match _l with
  | [] -> []
  | x :: xs ->
      if x = _n then drop_leading _n xs
      else _l

let drop_trailing (_n : int) (_l : int list) : int list =
  let rev = List.rev _l in
  let dropped = drop_leading _n rev in
  List.rev dropped

let split_on_char (_c : char) (_s : string) : string list =
  let length = String.length _s in
  let rec loop i startPoint list =
    if i = length then
      let lastPoint = String.sub _s startPoint (length - startPoint) in
      List.rev (lastPoint :: list)
    else if String.get _s i = _c then
      let stringPiece = String.sub _s startPoint (i - startPoint) in
      loop (i + 1) (i + 1) (stringPiece :: list)
    else
      loop (i + 1) startPoint list
  in
  loop 0 0 []

let parse_fractran (_input : string) : Q.t list =
  let tokens = split_on_char ' ' _input in
  let to_frac (tok : string) : Q.t =
    let parts = split_on_char '/' tok in
    let numerator = Z.of_string (List.nth parts 0) in
    let denominator = Z.of_string (List.nth parts 1) in
    Q.make numerator denominator
  in
  List.map to_frac tokens

let eval_fractran (_program : Q.t list) (_input : Z.t) : Z.t =
  let rec run n =
    let rec try_fracs fractrans =
      match fractrans with
      | [] -> n
      | x :: xs ->
          let product = Q.mul x (Q.make n Z.one) in
          if Q.den product = Z.one then
            run (Q.num product)
          else
            try_fracs xs
    in
    try_fracs _program
  in
  run _input

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
