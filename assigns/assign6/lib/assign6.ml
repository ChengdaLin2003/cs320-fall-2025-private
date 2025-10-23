type expr =
  | True
  | False
  | Int of int
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Lte of expr * expr
  | If of expr * expr * expr
  | Let of string * expr * expr

let is_lowercase (c : char) : bool =
  c >= 'a' && c <= 'z'

let is_valid_var : string -> bool= String.for_all is_lowercase

let parse (s : string) : expr option =
    (* This helper splits the input string into tokens. *)
    let tokenize (s : string) : string list =
    let is_space c = c = ' ' || c = '\n' || c = '\t' || c = '\r' in
    let acc = ref [] in
    let buf = Buffer.create 32 in
    let push_buf () =
      if Buffer.length buf > 0 then (
        acc := Buffer.contents buf :: !acc;
        Buffer.clear buf
      )
    in
    let push t = acc := t :: !acc in
    let n = String.length s in
    let rec loop i =
      if i >= n then (push_buf (); List.rev !acc) else
      let c = s.[i] in
      if is_space c then (push_buf (); loop (i+1))
      else if c = '(' then (push_buf (); push "("; loop (i+1))
      else if c = ')' then (push_buf (); push ")"; loop (i+1))
      else (Buffer.add_char buf c; loop (i+1))
    in
    loop 0
  in
  (* This recursive function tries to parse an expression from the token list. *)
  let rec parse_expr (ts : string list) : (expr * string list) option =
    match ts with
    | [] -> None

    | "True" :: rest  -> Some (True, rest)
    | "False" :: rest -> Some (False, rest)

    (* If the token is an integer, parse it as Int. *)
    | tok :: rest when Stdlib.int_of_string_opt tok <> None ->
        Some (Int (Stdlib.int_of_string tok), rest)

    (* If the token is a valid variable name, parse as Var. *)
    | tok :: rest when is_valid_var tok ->
        Some (Var tok, rest)

    (* If we see an opening parenthesis, we expect an operator or special form next. *)
    | "(" :: op :: rest -> begin
        match op with
        (* Here we handle binary operators like + and - *)
        | "+"  -> parse2 rest (fun a b -> Add (a, b))
        | "-"  -> parse2 rest (fun a b -> Sub (a, b))
        | "*"  -> parse2 rest (fun a b -> Mul (a, b))
        | "/"  -> parse2 rest (fun a b -> Div (a, b))
        | "<=" -> parse2 rest (fun a b -> Lte (a, b))
        (* For If, we parse three expressions and expect a closing parenthesis. *)
        | "If" -> begin
            match parse_expr rest with
            | Some (e1, rest1) ->
              (match parse_expr rest1 with
               | Some (e2, rest2) ->
                 (match parse_expr rest2 with
                  | Some (e3, ")" :: rest3) -> Some (If (e1, e2, e3), rest3)
                  | _ -> None)
               | _ -> None)
            | _ -> None
          end
        (* For Let, we parse a variable, its binding expression, then the body, expecting a closing parenthesis. *)
        | "Let" -> begin
            match rest with
            | x :: rest1 when is_valid_var x ->
              (match parse_expr rest1 with
               | Some (e_bind, rest2) ->
                 (match parse_expr rest2 with
                  | Some (e_body, ")" :: rest3) -> Some (Let (x, e_bind, e_body), rest3)
                  | _ -> None)
               | _ -> None)
            | _ -> None
          end
        | _ -> None
      end

    | _ -> None

  (* Helper to parse two expressions followed by a closing parenthesis for binary ops. *)
  and parse2 (ts : string list) (mk : expr -> expr -> expr)
      : (expr * string list) option =
    match parse_expr ts with
    | Some (e1, rest1) ->
      (match parse_expr rest1 with
       | Some (e2, ")" :: rest2) -> Some (mk e1 e2, rest2)
       | _ -> None)
    | _ -> None
  in
  let toks = tokenize s in
  match parse_expr toks with
  | Some (e, []) -> Some e
  | _ -> None 

type ty =
  | IntTy
  | BoolTy

module M = Map.Make(String)
type ctxt = ty M.t

let empty_ctxt : ctxt = M.empty
let add_binding (x : string) (t : ty) (gamma : ctxt) : ctxt = M.add x t gamma
let check_binding (x : string) (gamma : ctxt) : ty option = M.find_opt x gamma

let rec type_of (gamma : ctxt) (e : expr) : ty option =
    match e with
  | True  -> Some BoolTy
  | False -> Some BoolTy
  | Int _ -> Some IntTy
  | Var x -> check_binding x gamma

  | Add (e1, e2)
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2) ->
      begin
          (* Arithmetic operations require both operands to be Int, result type is Int *)
        match type_of gamma e1, type_of gamma e2 with
        | Some IntTy, Some IntTy -> Some IntTy
        | _ -> None
      end

  | Lte (e1, e2) ->
      begin
          (* Less-than-or-equal requires Int operands, returns Bool type *)
        match type_of gamma e1, type_of gamma e2 with
        | Some IntTy, Some IntTy -> Some BoolTy
        | _ -> None
      end

  | If (e1, e2, e3) ->
      begin
          (* If expression: guard must be Bool, then both branches must have the same type *)
        match type_of gamma e1 with
        | Some BoolTy ->
            begin
              match type_of gamma e2, type_of gamma e3 with
              | Some t2, Some t3 when t2 = t3 -> Some t2
              | _ -> None
            end
        | _ -> None
      end

  | Let (x, e_bind, e_body) ->
      begin
          (* Let binding: type check e_bind to get type tx, extend context with x:tx and type check body *)
        match type_of gamma e_bind with
        | None -> None
        | Some tx ->
            let gamma' = add_binding x tx gamma in
            type_of gamma' e_body
      end

let subst (e1 : expr) (x : string) (e2 : expr) : expr =
  let rec go e =
    match e with
    | True -> True
    | False -> False
    | Int n -> Int n
    | Var y -> if y = x then e1 else Var y
    | Add (e1, e2) -> Add (go e1, go e2)
    | Sub (e1, e2) -> Sub (go e1, go e2)
    | Mul (e1, e2) -> Mul (go e1, go e2)
    | Div (e1, e2) -> Div (go e1, go e2)
    | Lte (e1, e2) -> Lte (go e1, go e2)
    | If (e1, e2, e3) -> If (go e1, go e2, go e3)
    | Let (y, e1, e2) -> Let (y, go e1, if y = x then e2 else go e2)
  in go e2

type value =
  | IntV of int
  | BoolV of bool

let expr_of_value (v : value) : expr =
  match v with
  | IntV n -> Int n
  | BoolV true -> True
  | BoolV false -> False

let rec eval (e : expr) : value =
  match e with
  | True -> BoolV true
  | False -> BoolV false
  | Int n -> IntV n
  | Var _ -> assert false
  | Add (e1, e2) -> (  (* Evaluate both sides, then add *)
      match eval e1, eval e2 with
      | IntV n1, IntV n2 -> IntV (n1 + n2)
      | _ -> assert false
    )
  | Sub (e1, e2) -> (  (* Evaluate both sides, then subtract *)
      match eval e1, eval e2 with
      | IntV n1, IntV n2 -> IntV (n1 - n2)
      | _ -> assert false
    )
  | Mul (e1, e2) -> (  (* Evaluate both sides, then multiply *)
      match eval e1, eval e2 with
      | IntV n1, IntV n2 -> IntV (n1 * n2)
      | _ -> assert false
    )
  | Div (e1, e2) -> (  (* Evaluate both sides, then divide (integer division) *)
      match eval e1, eval e2 with
      | IntV n1, IntV n2 -> IntV (n1 / n2)
      | _ -> assert false
    )
  | Lte (e1, e2) -> (  (* Evaluate both sides, then compare, returning BoolV *)
      match eval e1, eval e2 with
      | IntV n1, IntV n2 -> BoolV (n1 <= n2)
      | _ -> assert false
    )
  | If (e1, e2, e3) -> (  (* Evaluate guard first, then evaluate chosen branch *)
      match eval e1 with
      | BoolV true -> eval e2
      | BoolV false -> eval e3
      | _ -> assert false
    )
  | Let (x, e1, e2) ->  (* Evaluate binding, substitute value-as-expr, then continue *)
      let v1 = eval e1 in
      let e' = subst (expr_of_value v1) x e2 in
      eval e'

let interp ?(print=true) (s : string) : value option =
  let print s = if print then print_endline s else () in
  let _ = print "parsing..." in
  match parse s with
  | None -> None
  | Some e ->
    let _ = print "type_checking..." in
    match type_of empty_ctxt e with
    | None -> None
    | Some _ ->
      let _ = print "evaluating..." in
      Some (eval e)
