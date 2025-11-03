include Utils

let parse (s : string) : expr option =
  let lb = Lexing.from_string s in
  try Some (Parser.prog Lexer.read lb) with Parser.Error -> None

let expr_of_value = function
  | VNum n -> Num n
  | VBool b -> if b then True else False
  | VUnit -> Unit
  | VFun (x, body) -> Fun (x, body)

let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Unit | True | False | Num _ -> e
  | Var y -> if y = x then expr_of_value v else e
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (c, t, f) -> If (subst v x c, subst v x t, subst v x f)
  | Let (y, e1, e2) ->
      if y = x then Let (y, subst v x e1, e2)
      else Let (y, subst v x e1, subst v x e2)
  | Fun (y, body) -> if y = x then Fun (y, body) else Fun (y, subst v x body)

let expect_num = function VNum n -> Some n | _ -> None

let rec eval (e : expr) : (value, error) result =
  match e with
  | Unit -> Ok VUnit
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Num n -> Ok (VNum n)
  | Var x -> Error (UnknownVar x)
  | Fun (x, body) -> Ok (VFun (x, body))
  | App (e1, e2) -> (
      match eval e1 with
      | Ok (VFun (x, body)) -> (
          match eval e2 with
          | Ok v2 -> eval (subst v2 x body)
          | Error _ as err -> err)
      | Ok _ -> Error InvalidApp
      | Error _ as err -> err)
  | Let (x, e1, e2) -> (
      match eval e1 with
      | Ok v -> eval (subst v x e2)
      | Error _ as err -> err)
  | If (c, t, f) -> (
      match eval c with
      | Ok (VBool true) -> eval t
      | Ok (VBool false) -> eval f
      | Ok _ -> Error InvalidIfCond
      | Error _ as err -> err)
  | Bop (op, e1, e2) -> (
      match op with
      | And -> (
          match eval e1 with
          | Ok (VBool false) -> Ok (VBool false)
          | Ok (VBool true) -> (
              match eval e2 with
              | Ok (VBool b2) -> Ok (VBool b2)
              | Ok _ -> Error (InvalidArgs And)
              | Error _ as err -> err)
          | Ok _ -> Error (InvalidArgs And)
          | Error _ as err -> err)
      | Or -> (
          match eval e1 with
          | Ok (VBool true) -> Ok (VBool true)
          | Ok (VBool false) -> (
              match eval e2 with
              | Ok (VBool b2) -> Ok (VBool b2)
              | Ok _ -> Error (InvalidArgs Or)
              | Error _ as err -> err)
          | Ok _ -> Error (InvalidArgs Or)
          | Error _ as err -> err)
      | Add | Sub | Mul | Div | Mod | Lt | Lte | Gt | Gte | Eq | Neq -> (
          match eval e1 with
          | Error _ as err -> err
          | Ok v1 -> (
              match eval e2 with
              | Error _ as err -> err
              | Ok v2 -> (
                  match op with
                  | Add -> (match expect_num v1, expect_num v2 with
                            | Some n1, Some n2 -> Ok (VNum (n1 + n2))
                            | _ -> Error (InvalidArgs Add))
                  | Sub -> (match expect_num v1, expect_num v2 with
                            | Some n1, Some n2 -> Ok (VNum (n1 - n2))
                            | _ -> Error (InvalidArgs Sub))
                  | Mul -> (match expect_num v1, expect_num v2 with
                            | Some n1, Some n2 -> Ok (VNum (n1 * n2))
                            | _ -> Error (InvalidArgs Mul))
                  | Div -> (match expect_num v1, expect_num v2 with
                            | Some _, Some 0 -> Error DivByZero
                            | Some n1, Some n2 -> Ok (VNum (n1 / n2))
                            | _ -> Error (InvalidArgs Div))
                  | Mod -> (match expect_num v1, expect_num v2 with
                            | Some _, Some 0 -> Error DivByZero
                            | Some n1, Some n2 -> Ok (VNum (n1 mod n2))
                            | _ -> Error (InvalidArgs Mod))
                  | Lt  -> (match expect_num v1, expect_num v2 with
                            | Some n1, Some n2 -> Ok (VBool (n1 < n2))
                            | _ -> Error (InvalidArgs Lt))
                  | Lte -> (match expect_num v1, expect_num v2 with
                            | Some n1, Some n2 -> Ok (VBool (n1 <= n2))
                            | _ -> Error (InvalidArgs Lte))
                  | Gt  -> (match expect_num v1, expect_num v2 with
                            | Some n1, Some n2 -> Ok (VBool (n1 > n2))
                            | _ -> Error (InvalidArgs Gt))
                  | Gte -> (match expect_num v1, expect_num v2 with
                            | Some n1, Some n2 -> Ok (VBool (n1 >= n2))
                            | _ -> Error (InvalidArgs Gte))
                  | Eq  -> (match v1, v2 with
                            | VNum n1, VNum n2 -> Ok (VBool (n1 = n2))
                            | VBool b1, VBool b2 -> Ok (VBool (b1 = b2))
                            | VUnit, VUnit -> Ok (VBool true)
                            | _ -> Error (InvalidArgs Eq))
                  | Neq -> (match v1, v2 with
                            | VNum n1, VNum n2 -> Ok (VBool (n1 <> n2))
                            | VBool b1, VBool b2 -> Ok (VBool (b1 <> b2))
                            | VUnit, VUnit -> Ok (VBool false)
                            | _ -> Error (InvalidArgs Neq))
                  | And | Or -> assert false))))

let interp (s : string) : (value, error) result =
  match parse s with
  | None -> Error ParseFail
  | Some e -> eval e
