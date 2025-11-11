(* Bring in all AST types and utility definitions *)
include Utils

(* -------------------- Parsing -------------------- *)
(** [parse s] turns string [s] into a program AST (prog option).
    It calls the lexer and parser, returning [Some ast] on success
    and [None] if any exception (parse error) occurs. *)
let parse (s : string) : prog option =
  try
    let lexbuf = Lexing.from_string s in
    Some (Parser.prog Lexer.read lexbuf)
  with _ -> None

(* -------------------- Values <-> Syntax -------------------- *)

(** [expr_of_value v] converts a runtime value back to a surface expression.
    This is only used in substitution and never printed to the user. *)
let expr_of_value (v : value) : expr =
  match v with
  | VNum n      -> Num n
  | VBool true  -> True
  | VBool false -> False
  | VFun (x, e) -> Fun (x, e)
  | VUnit       -> Unit

(* -------------------- Substitution -------------------- *)

(** [subst v x e] performs capture-avoiding substitution [v/x]e:
    Replace all *free* occurrences of variable [x] in [e] with [v].
    Since values contain no free variables (they are closed),
    it’s sufficient to stop at binders that shadow [x]. *)
let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Unit | True | False | Num _ -> e
  | Var y -> if y = x then expr_of_value v else e

  | Let (y, e1, e2) ->
      let e1' = subst v x e1 in
      if y = x then Let (y, e1', e2)           (* [y] shadows [x] in e2 *)
      else Let (y, e1', subst v x e2)

  | If (c, t, f) ->
      If (subst v x c, subst v x t, subst v x f)

  | Fun (y, body) ->
      if y = x then e                           (* [y] shadows [x] in body *)
      else Fun (y, subst v x body)

  | App (e1, e2) ->
      App (subst v x e1, subst v x e2)

  | Bop (op, e1, e2) ->
      Bop (op, subst v x e1, subst v x e2)

(* -------------------- Small helpers for evaluation -------------------- *)

let invalid_args (op : bop) : ('a, error) result = Error (InvalidArgs op)

(** Expect numeric operand; otherwise return InvalidArgs. *)
let expect_num op = function
  | VNum n -> Ok n
  | _      -> invalid_args op

(** Expect boolean operand; otherwise return InvalidArgs. *)
let expect_bool op = function
  | VBool b -> Ok b
  | _       -> invalid_args op

(** Arithmetic binary operations on integers. *)
let bin_num op n1 n2 =
  match op with
  | Add -> Ok (VNum (n1 + n2))
  | Sub -> Ok (VNum (n1 - n2))
  | Mul -> Ok (VNum (n1 * n2))
  | Div ->
      if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
  | Mod ->
      if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
  | _ -> assert false

(** Numeric comparison to boolean. *)
let comp_num op n1 n2 =
  let b =
    match op with
    | Lt  -> n1 <  n2
    | Lte -> n1 <= n2
    | Gt  -> n1 >  n2
    | Gte -> n1 >= n2
    | _   -> assert false
  in
  Ok (VBool b)

(** Equality/inequality on ints only (as per our language). *)
let eq_neq op v1 v2 =
  match v1, v2 with
  | VNum n1, VNum n2 ->
      let b = match op with Eq -> (n1 = n2) | Neq -> (n1 <> n2) | _ -> assert false in
      Ok (VBool b)
  | _ -> invalid_args op

(* -------------------- Desugaring -------------------- *)
(** 把语法糖改写成核心子集；保证与规范一致的求值顺序与短路。
    - AND/OR 用 If 改写以实现短路
    - Lte/Gte/Neq 改写成原子比较 + If/Or
    - 其它保持原样
    （一元负号已在 parser 阶段 desugar 成 0 - x） *)
let rec desugar (e : expr) : expr =
  match e with
  | Unit | True | False | Num _ | Var _ -> e

  | Let (x, e1, e2) ->
      Let (x, desugar e1, desugar e2)

  | If (c, t, f) ->
      If (desugar c, desugar t, desugar f)

  | Fun (x, body) ->
      Fun (x, desugar body)

  | App (e1, e2) ->
      App (desugar e1, desugar e2)

  | Bop (op, e1, e2) ->
      let d1 = desugar e1 in
      let d2 = desugar e2 in
      begin match op with
      (* 短路语义 *)
      | And -> If (d1, d2, False)
      | Or  -> If (d1, True, d2)

      (* 复合比较改写 *)
      | Lte ->
          (* a <= b  ⇝  (a < b) || (a = b) *)
          desugar (Bop (Or, Bop (Lt, d1, d2), Bop (Eq, d1, d2)))
      | Gte ->
          (* a >= b  ⇝  (a > b) || (a = b) *)
          desugar (Bop (Or, Bop (Gt, d1, d2), Bop (Eq, d1, d2)))
      | Neq ->
          (* a <> b  ⇝  if a = b then false else true *)
          If (Bop (Eq, d1, d2), False, True)

      (* 原子运算原样保留 *)
      | Add | Sub | Mul | Div | Mod
      | Lt | Gt | Eq -> Bop (op, d1, d2)
      end

(* -------------------- Evaluation -------------------- *)

(** [eval e] evaluates [e] to a value or an error. *)
let rec eval (e : expr) : (value, error) result =
  match e with
  | Unit -> Ok VUnit
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Num n -> Ok (VNum n)

  | Var x -> Error (UnknownVar x)   (* no explicit env; free vars are errors *)

  | Let (x, e1, e2) -> (
      match eval e1 with
      | Error _ as e -> e
      | Ok v1 -> eval (subst v1 x e2)
    )

  | If (c, t, f) -> (
      match eval c with
      | Ok (VBool b) -> if b then eval t else eval f
      | Ok _         -> Error InvalidIfCond
      | Error _ as e -> e
    )

  | Fun (x, body) -> Ok (VFun (x, body))

  | App (e1, e2) -> (
      match eval e1 with
      | Ok (VFun (x, body)) -> (
          match eval e2 with
          | Error _ as err -> err
          | Ok v2 -> eval (subst v2 x body))
      | Ok _         -> Error InvalidApp
      | Error _ as e -> e
    )

  | Bop (op, e1, e2) -> (
      match op with
      (* 如果 desugar 没把 And/Or 改写掉，这里也支持短路，双保险 *)
      | And -> (
          match eval e1 with
          | Ok (VBool false) -> Ok (VBool false)
          | Ok (VBool true)  -> eval e2
          | Ok _             -> invalid_args And
          | Error _ as e     -> e
        )
      | Or -> (
          match eval e1 with
          | Ok (VBool true)  -> Ok (VBool true)
          | Ok (VBool false) -> eval e2
          | Ok _             -> invalid_args Or
          | Error _ as e     -> e
        )

      (* 数值算术 *)
      | Add | Sub | Mul | Div | Mod -> (
          match eval e1 with
          | Error _ as e -> e
          | Ok v1 -> (
              match eval e2 with
              | Error _ as e -> e
              | Ok v2 ->
                  (match expect_num op v1, expect_num op v2 with
                   | Ok n1, Ok n2 -> bin_num op n1 n2
                   | _    , _     -> invalid_args op))
        )

      (* 比较 *)
      | Lt | Lte | Gt | Gte -> (
          match eval e1 with
          | Error _ as e -> e
          | Ok v1 -> (
              match eval e2 with
              | Error _ as e -> e
              | Ok v2 ->
                  (match expect_num op v1, expect_num op v2 with
                   | Ok n1, Ok n2 -> comp_num op n1 n2
                   | _    , _     -> invalid_args op))
        )

      (* 相等/不等（仅对整数） *)
      | Eq | Neq -> (
          match eval e1 with
          | Error _ as e -> e
          | Ok v1 -> (
              match eval e2 with
              | Error _ as e -> e
              | Ok v2 -> eq_neq op v1 v2)
        )
    )

(* -------------------- Top-level Entry -------------------- *)

(** [interp s] runs the entire pipeline:
    - Parse string [s] into an AST
    - Desugar it (remove syntactic sugar)
    - Evaluate it
    - Return either [Ok value] or [Error reason] *)
let interp (s : string) : (value, error) result =
  match parse s with
  | None   -> Error ParseFail
  | Some e -> eval (desugar e)