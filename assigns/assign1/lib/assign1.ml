let num_factors (_n : int) : int =
    let rec counter n d count =
        if d * d > n then
            if n > 1 then count + 1 else count
        else if n mod d = 0 then
            counter (n/d) d (count + 1)
        else
            counter n (d + 1) count
        in
        counter _n 2 0;;

let perfect_power (i : int) (n : int) : bool =
  if i <= 0 then false
  else if n = 0 then true
  else if n = 1 then true
  else if n = -1 then (i mod 2 = 1)
  else if n < 0 && i mod 2 = 0 then false
  else
    let abs_n = abs n in
    let rec pow_bounded (b : int) (e : int) (limit : int) : int =
      if e = 0 then 1
      else if b = 0 then 0
      else
        let p = pow_bounded b (e - 1) limit in
        if p > limit / b then limit + 1 else b * p
    in
    let upper =
      int_of_float ((float_of_int abs_n) ** (1.0 /. float_of_int i)) + 1
    in
    let rec check k =
      if k > upper then false
      else
        let p = pow_bounded k i abs_n in
        if p = abs_n then true else check (k + 1)
    in
    check 0

let rec collatz (n : int) : int =
  if n = 1 then 0
  else if n mod 2 = 0 then 1 + collatz (n / 2)
  else 1 + collatz (3 * n + 1)

let tst_records (i : int) : int =
    let rec loop k best found =
      let steps = collatz k in
      if steps > best then
        if found = i then k
        else loop (k + 1) steps (found + 1)
      else
        loop (k + 1) best found
    in
    loop 1 (-1) 0