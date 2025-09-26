
let drop_leading (c : char) (s : string) : string =
  let rec go i =
    if i >= String.length s
    then ""
    else if String.get s i = c
    then go (i + 1)
    else String.sub s i (String.length s - i)
  in go 0

let drop_trailing (c : char) (s : string) : string =
  let rec go i =
    if i < 0
    then ""
    else if String.get s i = c
    then go (i - 1)
    else String.sub s 0 (i + 1)
  in go (String.length s - 1)
