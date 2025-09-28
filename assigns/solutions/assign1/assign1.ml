
let num_factors (n : int) : int =
  let rec go (count : int) (i : int) (k : int) : int =
    if k = 1
    then count
    else if k mod i = 0
    then go (count + 1) i (k / i)
    else go count (i + 1) k
  in go 0 2 n

let perfect_power (i : int) (n : int) : bool =
  let pow (n : int) (k : int) : int =
    let rec go (acc : int) (k : int) : int =
      if k = 0
      then acc
      else go (acc * n) (k - 1)
    in go 1 k
  in
  if n = 1
  then true
  else if i <= 0
  then false
  else if n < 0 && i mod 2 = 0
  then false
  else
    let n = abs n in
    let rec go k =
      let k_to_the_i = pow k i in
      if k_to_the_i < n
      then go (k + 1)
      else n = k_to_the_i
    in
    go 0

let collatz (n : int) : int =
  let rec go (count : int) (n : int) : int =
    if n = 1
    then count
    else
      go
        (count + 1)
        (if n mod 2 = 0 then n / 2 else 3 * n + 1)
  in go 0 n

let tst_records (i : int) : int =
  let rec go (max : int) (n : int) (i : int) : int =
    if i = 0
    then n - 1
    else
      let collatz = collatz n in
      if collatz > max
      then go collatz (n + 1) (i - 1)
      else go max (n + 1) i
  in go 0 2 i
