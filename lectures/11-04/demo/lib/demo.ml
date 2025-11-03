include Utils

let parse (s : string) : expr option =
  match Parser.prog Lexer.read (Lexing.from_string s) with
  | e -> Some e
  | exception _ -> None

(*
   Semantics:

   n is an int
   ─────────── (eval-int)
     n ⇓ n

   ───────────────────── (eval-int)
   fun x -> e ⇓ λ x . e

   e₁ ⇓ λ x . e    e₂ ⇓ v₂    [v₂ / x]e ⇓ v
   ────────────────────────────────────────── (eval-int)
                  e₁ e₂ ⇓ v



   Substituion (ASSUMING v HAS NO FREE VARIABLES):

   [v / x]n            =  n                      n is an int
   [v / x]y            =  v                      x = y
                       &  y                      otherwise
   [v / x](fun y -> e) = fun y -> e              x = y
                       & fun y -> [v / x]e       otherwise
   [v / x](e₁ e₂)      = ([v / x]e₁)([v / x]e₂)



   Free Variables:

   fv(n)     = ∅  (n is an int)
   fv(x)     = {x}
   fv(λx.e)  = fv(e) \ {x}
   fv(e₁ e₂) = fv(e₁) ∪ fv(e₂)
*)

let expr_of_value (v : value) : expr =
  match v with
  | IntV n -> Int n
  | FunV (x, e) -> Fun (x, e)

let rec subst (v : value) (x : string) (e : expr) : expr =
  match e with
  | Int n -> Int n
  | Var y ->
    if x = y
    then expr_of_value v
    else Var y
  | Fun (y, e) ->
    if x = y
    then Fun (y, e)
    else Fun (y, subst v x e)
  | App (e1, e2) -> App (subst v x e1, subst v x e2)

let rec eval (e : expr) : value option =
  match e with
  | Int n -> Some (IntV n)
  | Var _ -> None
  | Fun (x, e) -> Some (FunV (x, e))
  | App (e1, e2) -> (
    match (eval e1) with
    | Some (FunV (x, e)) -> (
      match eval e2 with
      | Some v2 -> eval (subst v2 x e)
      | _ -> None
    )
    | _ -> None
  )

let rec fv (e : expr) : string list =
  match e with
  | Int _ -> []
  | Var x -> [x]
  | Fun (x, e) -> List.filter ((<>) x) (fv e)
  | App (e1, e2) -> fv e1 @ fv e2

let is_well_scoped (e : expr) : bool = List.is_empty (fv e)

let interp (p : string) : value option =
  match parse p with
  | Some e ->
    if is_well_scoped e
    then eval e
    else None
  | None -> None
