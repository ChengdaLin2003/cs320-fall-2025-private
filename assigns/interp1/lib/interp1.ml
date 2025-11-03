(* re-export AST constructors so tests that `open Interp1` can see Bop/Num/etc. *)
include Utils

(* -------------------- parse -------------------- *)

let parse (s : string) : prog option =
  try
    let lexbuf = Lexing.from_string s in
    Some (Parser.prog Lexer.read lexbuf)
  with _ -> None

(* -------------------- substitution -------------------- *)

let expr_of_value (v : value) : expr =
  match v with
  | VNum n      -> Num n
  | VBool true  -> True
  | VBool false -> False
  | VFun (x, e) -> Fun (x, e)
  | VUnit       -> Unit

(* capture-avoiding substitute [v/x]e; since our values contain only closed terms
   (numbers/bools/unit and a syntactic Fun), simple binder checks are enough *)
let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Unit | True | False | Num _ -> e
  | Var y -> if y = x then expr_of_value v else e
  | Let (y, e1, e2) ->
      let e1' = subst v x e1 in
      if y = x then Let (y, e1', e2)      (* y shadows x in e2 *)
      else Let (y, e1', subst v x e2)
  | If (c, t, f) ->
      If (subst v x c, subst v x t, subst v x f)
  | Fun (y, body) ->
      if y = x then e                      (* y shadows x in body *)
      else Fun (y, subst v x body)
  | App (e1, e2) ->
      App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) ->
      Bop (op, subst v x e1, subst v x e2)

(* -------------------- evaluation -------------------- *)

let invalid_args op = Error (InvalidArgs op)

let expect_num op = function
  | VNum n -> Ok n
  | _      -> invalid_args op

let expect_bool op = function
  | VBool b -> Ok b
  | _       -> invalid_args op

let bin_num op n1 n2 =
  match op with
  | Add -> Ok (VNum (n1 + n2))
  | Sub -> Ok (VNum (n1 - n2))
  | Mul -> Ok (VNum (n1 * n2))
  | Div -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 / n2))
  | Mod -> if n2 = 0 then Error DivByZero else Ok (VNum (n1 mod n2))
  | _   -> assert false

let bin_cmp op n1 n2 =
  let b =
    match op with
    | Lt  -> n1 <  n2
    | Lte -> n1 <= n2
    | Gt  -> n1 >  n2
    | Gte -> n1 >= n2
    | _   -> assert false
  in Ok (VBool b)

let eq_neq op v1 v2 =
  (* 支持 int/bool/unit 的相等；函数或跨类型相等视为 InvalidArgs *)
  match (v1, v2) with
  | VNum a,  VNum b  -> Ok (VBool (if op = Eq then a = b else a <> b))
  | VBool a, VBool b -> Ok (VBool (if op = Eq then a = b else a <> b))
  | VUnit,  VUnit    -> Ok (VBool (op = Eq))
  | _ -> invalid_args op

let rec eval (e : expr) : (value, error) result =
  match e with
  | Unit  -> Ok VUnit
  | True  -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Num n -> Ok (VNum n)
  | Var x -> Error (UnknownVar x)

  | Let (x, e1, e2) -> (
      match eval e1 with
      | Error _ as err -> err
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
      | And -> (* 短路与 *)
          (match eval e1 with
           | Ok (VBool b1) ->
               if not b1 then Ok (VBool false)
               else (match eval e2 with
                     | Ok (VBool b2) -> Ok (VBool b2)
                     | Ok _          -> invalid_args And
                     | Error _ as e  -> e)
           | Ok _         -> invalid_args And
           | Error _ as e -> e)
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
      | Eq | Neq ->
          (match eval e1 with
           | Error _ as e -> e
           | Ok v1 ->
             match eval e2 with
             | Error _ as e -> e
             | Ok v2 -> eq_neq op v1 v2)
    )

(* -------------------- top-level -------------------- *)

let interp (s : string) : (value, error) result =
  match parse s with
  | None   -> Error ParseFail
  | Some e -> eval e