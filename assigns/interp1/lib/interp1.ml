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


(* -------------------- Substitution -------------------- *)

(** Convert a runtime value back into a syntactic expression.
    Used during substitution when replacing variable occurrences. *)
let expr_of_value (v : value) : expr =
  match v with
  | VNum n      -> Num n
  | VBool true  -> True
  | VBool false -> False
  | VFun (x, e) -> Fun (x, e)
  | VUnit       -> Unit

(** [subst v x e] performs capture-avoiding substitution [v/x]e:
    Replace all *free* occurrences of variable [x] in [e] with [v].
    Since values contain no free variables (they are closed),
    it’s sufficient to skip substitution under a re-binding of [x]. *)
let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Unit | True | False | Num _ -> e
  | Var y -> if y = x then expr_of_value v else e
  | Let (y, e1, e2) ->
      let e1' = subst v x e1 in
      if y = x then Let (y, e1', e2)    (* [y] shadows [x] in e2 *)
      else Let (y, e1', subst v x e2)
  | If (c, t, f) ->
      If (subst v x c, subst v x t, subst v x f)
  | Fun (y, body) ->
      if y = x then e                   (* [y] shadows [x] in body *)
      else Fun (y, subst v x body)
  | App (e1, e2) ->
      App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) ->
      Bop (op, subst v x e1, subst v x e2)


(* -------------------- Evaluation -------------------- *)

(** Helper for reporting invalid argument types for operator [op]. *)
let invalid_args op = Error (InvalidArgs op)

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
  | Div -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
  | Mod -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
  | _   -> assert false

(** Comparison binary operators (<, <=, >, >=). *)
let bin_cmp op n1 n2 =
  let b =
    match op with
    | Lt  -> n1 <  n2
    | Lte -> n1 <= n2
    | Gt  -> n1 >  n2
    | Gte -> n1 >= n2
    | _   -> assert false
  in Ok (VBool b)

(** Equality and inequality. 
    Only numbers, bools, and units can be compared.
    Functions or mismatched types cause InvalidArgs. *)
let eq_neq op v1 v2 =
  match (v1, v2) with
  | VNum a,  VNum b  -> Ok (VBool (if op = Eq then a = b else a <> b))
  | VBool a, VBool b -> Ok (VBool (if op = Eq then a = b else a <> b))
  | VUnit,  VUnit    -> Ok (VBool (op = Eq))
  | _ -> invalid_args op


(* -------------------- Interpreter Core -------------------- *)

(** Big-step (call-by-value) evaluator.
    Returns [Ok v] for success, or [Error e] for runtime error. *)
let rec eval (e : expr) : (value, error) result =
  match e with
  | Unit  -> Ok VUnit
  | True  -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Num n -> Ok (VNum n)
  | Var x -> Error (UnknownVar x)

  (* let x = e1 in e2 *)
  | Let (x, e1, e2) -> (
      match eval e1 with
      | Error _ as err -> err
      | Ok v1 -> eval (subst v1 x e2)
    )

  (* if c then t else f *)
  | If (c, t, f) -> (
      match eval c with
      | Ok (VBool b) -> if b then eval t else eval f
      | Ok _         -> Error InvalidIfCond
      | Error _ as e -> e
    )

  (* function abstraction *)
  | Fun (x, body) -> Ok (VFun (x, body))

  (* function application *)
  | App (e1, e2) -> (
      match eval e1 with
      | Ok (VFun (x, body)) -> (
          match eval e2 with
          | Error _ as err -> err
          | Ok v2 -> eval (subst v2 x body))
      | Ok _         -> Error InvalidApp
      | Error _ as e -> e
    )

  (* binary operators *)
  | Bop (op, e1, e2) -> (
      match op with
      (* Boolean AND with short-circuiting *)
      | And ->
          (match eval e1 with
           | Ok (VBool b1) ->
               if not b1 then Ok (VBool false)
               else (match eval e2 with
                     | Ok (VBool b2) -> Ok (VBool b2)
                     | Ok _          -> invalid_args And
                     | Error _ as e  -> e)
           | Ok _         -> invalid_args And
           | Error _ as e -> e)
      (* Boolean OR with short-circuiting *)
      | Or ->
          (match eval e1 with
           | Ok (VBool b1) ->
               if b1 then Ok (VBool true)
               else (match eval e2 with
                     | Ok (VBool b2) -> Ok (VBool b2)
                     | Ok _          -> invalid_args Or
                     | Error _ as e  -> e)
           | Ok _         -> invalid_args Or
           | Error _ as e -> e)
      (* Numeric arithmetic *)
      | Add | Sub | Mul | Div | Mod ->
          (match eval e1 with
           | Error _ as e -> e
           | Ok v1 ->
             match eval e2 with
             | Error _ as e -> e
             | Ok v2 ->
               (match expect_num op v1, expect_num op v2 with
                | Ok n1, Ok n2 -> bin_num op n1 n2
                | _            -> invalid_args op))
      (* Numeric comparisons *)
      | Lt | Lte | Gt | Gte ->
          (match eval e1 with
           | Error _ as e -> e
           | Ok v1 ->
             match eval e2 with
             | Error _ as e -> e
             | Ok v2 ->
               (match expect_num op v1, expect_num op v2 with
                | Ok n1, Ok n2 -> bin_cmp op n1 n2
                | _            -> invalid_args op))
      (* Equality / Inequality *)
      | Eq | Neq ->
          (match eval e1 with
           | Error _ as e -> e
           | Ok v1 ->
             match eval e2 with
             | Error _ as e -> e
             | Ok v2 -> eq_neq op v1 v2)
    )


(* -------------------- Top-level Entry -------------------- *)

(** [interp s] runs the entire pipeline:
    - Parse string [s] into an AST
    - Evaluate it
    - Return either [Ok value] or [Error reason] *)
let interp (s : string) : (value, error) result =
  match parse s with
  | None   -> Error ParseFail
  | Some e -> eval e