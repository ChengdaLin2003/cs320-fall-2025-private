
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let print_triples (n : int) : unit =
  let rec loop i j k =
    let _ =
      if i * i + j * j = k * k && gcd i j = 1
      then
        let line =
          string_of_int i
          ^ " "
          ^ string_of_int j
          ^ " "
          ^ string_of_int k
        in print_endline line
      else ()
    in
    if k < n
    then loop i j (k + 1)
    else if j < n
    then loop i (j + 1) (j + 1)
    else if i < n
    then loop (i + 1) (i + 1) (i + 1)
    else ()
  in loop 1 1 1

let repeat n c = String.init n (fun _ -> c)

let print_hour_glass (n : int) : unit =
  let rec go i n =
    if n = 0
    then print_endline (repeat i ' ' ^ "*")
    else
      let _ = print_endline (repeat i ' ' ^ repeat (2 * n + 1) '*') in
      let _ = go (i + 1) (n - 1) in
      let _ = print_endline (repeat i ' ' ^ repeat (2 * n + 1) '*') in
      ()
  in go 0 n
