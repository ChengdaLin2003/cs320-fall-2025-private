open Utils

let rec curry_fun (args : (string * ty) list) (body : expr) : expr =
  match args with
  | [] -> body
  | (x, tx) :: xs -> Fun (x, tx, curry_fun xs body)

let rec arrow_ty (args : (string * ty) list) (ret : ty) : ty =
  match args with
  | [] -> ret
  | (_, t) :: tl -> TFun (t, arrow_ty tl ret)

let desugar_one_top (t : toplet) (k : expr) : expr =
  let fun_e = curry_fun t.args t.body in
  let ann'  = arrow_ty t.args t.ann in
  if t.is_rec
  then LetRec (t.name, ann', fun_e, k)
  else Let    (t.name, ann', fun_e, k)

let desugar (p : prog) : expr =
  match p with
  | [] -> Unit
  | _  ->
      let last_name = (List.rev p |> List.hd).name in
      let ret = Var last_name in
      List.fold_right (fun top acc -> desugar_one_top top acc) p ret

let type_of (_ : expr) : (ty, error) result = Error ParseFail
let eval    (_ : expr) : value              = VUnit
let interp  (_ : string) : (value, error) result = Error ParseFail