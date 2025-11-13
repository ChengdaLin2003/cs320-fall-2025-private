open Utils

(* ---------- helpers used by desugar ---------- *)

(* 把 fun (x1:t1) (x2:t2) ... -> body
   翻译成 Fun(x1,t1, Fun(x2,t2, ... body)) 形式 *)
let rec curry_fun (args : (string * ty) list) (body : expr) : expr =
  match args with
  | [] -> body
  | (x, tx) :: xs -> Fun (x, tx, curry_fun xs body)

(* 把一个顶层 toplet 变成一个 Let/LetRec，并把“后续表达式”接在 body 上。
   注意：这里不再自己构造箭头类型，而是直接使用注解 ann。 *)
let desugar_one_top (t : toplet) (k : expr) : expr =
  let fun_e = curry_fun t.args t.body in
  let ty_annot = t.ann in
  if t.is_rec
  then LetRec (t.name, ty_annot, fun_e, k)
  else Let    (t.name, ty_annot, fun_e, k)

(* ---------- Part 1: desugar ---------- *)

(* prog 是顶层列表：
   [ let [rec] f1 ... ; let [rec] f2 ... ; ... ; let [rec] fn ... ]
   把它变成嵌套的：
   let [rec] f1 ... in
   let [rec] f2 ... in
   ...
   let [rec] fn ... in
   fn
*)
let desugar (p : prog) : expr =
  match p with
  | [] -> Unit
  | _  ->
      let last_name = (List.rev p |> List.hd).name in
      let ret = Var last_name in
      List.fold_right (fun top acc -> desugar_one_top top acc) p ret

(* ---------- stubs (check-in 用，保证能编译) ---------- *)

let type_of (_e : expr) : (ty, error) result = Error ParseFail
let eval    (_e : expr) : value              = VUnit
let interp  (_src : string) : (value, error) result = Error ParseFail