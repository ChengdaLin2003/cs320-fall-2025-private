open Interp2
open OUnit2

(* ---------- helpers for readable debugging ---------- *)
let rec string_of_expr = function
  | Unit -> "Unit"
  | True -> "True"
  | False -> "False"
  | Num n -> "Num " ^ string_of_int n
  | Var x -> "Var " ^ x
  | Let (x,e1,e2) ->
      "Let(" ^ x ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | If (c,t,f) ->
      "If(" ^ string_of_expr c ^ ", " ^ string_of_expr t ^ ", " ^ string_of_expr f ^ ")"
  | Fun (x,e) ->
      "Fun(" ^ x ^ ", " ^ string_of_expr e ^ ")"
  | App (e1,e2) ->
      "App(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Bop (op,e1,e2) ->
      let sop = match op with
        | Add->"Add"|Sub->"Sub"|Mul->"Mul"|Div->"Div"|Mod->"Mod"
        | Lt->"Lt"|Lte->"Lte"|Gt->"Gt"|Gte->"Gte"|Eq->"Eq"|Neq->"Neq"
        | And->"And"|Or->"Or"
      in
      "Bop(" ^ sop ^ ", " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

let assert_parse ?msg s expected =
  let actual = parse s in
  match actual, expected with
  | Some a, Some e when a = e -> ()
  | _ ->
      Printf.printf "PARSE DEBUG\n  input: %S\n  actual: %s\n  expect: %s\n%!"
        s
        (match actual with None -> "None" | Some a -> string_of_expr a)
        (match expected with None -> "None" | Some e -> string_of_expr e);
      assert_equal ?msg expected actual

(* ---------- parse tests ---------- *)
let parse_tests =
  "testing parse" >:::
    [
      "basic arithmetic expr" >:: (fun _ ->
        let expected = Some (Bop (Mul, Bop (Add, Num 1, Num 2), Num 3)) in
        assert_parse "(1 + 2) * 3" expected);

      "or left assoc" >:: (fun _ ->
        let expected = Some (Bop (Or, Bop (Or, True, False), False)) in
        assert_parse "true || false || false" expected);

      "and left assoc" >:: (fun _ ->
        let expected = Some (Bop (And, Bop (And, True, False), True)) in
        assert_parse "true && false && true" expected);

      "and binds tighter than or" >:: (fun _ ->
        let expected = Some (Bop (Or, True, Bop (And, False, True))) in
        assert_parse "true || false && true" expected);

      "comparisons bind tighter than and/or" >:: (fun _ ->
        let expected =
          Some
            (Bop
               ( Or,
                 Bop (And, Bop (Lt, Num 1, Num 2), Bop (Lt, Num 3, Num 4)),
                 Bop (Lt, Num 5, Num 6) ))
        in
        assert_parse "1 < 2 && 3 < 4 || 5 < 6" expected);

      "mul/div/mod bind tighter than add/sub" >:: (fun _ ->
        let expected =
          Some (Bop (Eq, Bop (Add, Num 1, Bop (Mul, Num 2, Num 3)), Num 7))
        in
        assert_parse "1 + 2 * 3 = 7" expected);

      "parens override precedence" >:: (fun _ ->
        let expected =
          Some (Bop (Eq, Bop (Mul, Bop (Add, Num 1, Num 2), Num 3), Num 9))
        in
        assert_parse "(1 + 2) * 3 = 9" expected);

      "subtraction left assoc" >:: (fun _ ->
        let expected = Some (Bop (Sub, Bop (Sub, Num 1, Num 2), Num 3)) in
        assert_parse "1 - 2 - 3" expected);

      "negative literal + mul/add" >:: (fun _ ->
        let expected =
          Some (Bop (Add, Bop (Mul, Bop (Sub, Num 0, Num 3), Num 4), Num 5))
        in
        assert_parse "-3 * 4 + 5" expected);

      "mixed or/and/comp" >:: (fun _ ->
        let expected =
          Some
            (Bop
               ( Or,
                 Bop (Or,
                      Bop (And, Bop (Lte, Num 1, Num 2), Bop (Gt, Num 3, Num 0)),
                      Bop (And, True, Bop (Eq, Num 4, Num 4))),
                 False ))
        in
        assert_parse "1 <= 2 && 3 > 0 || true && 4 = 4 || false" expected);
    ]

(* ---------- subst tests ---------- *)
let subst_tests =
  "testing subst" >:::
    [
      "single variable" >:: (fun _ ->
        let expected = Bop (Add, Var "x", If (Unit, Unit, Unit)) in
        let actual =
          subst VUnit "y" (Bop (Add, Var "x", If (Var "y", Var "y", Var "y")))
        in
        assert_equal expected actual);
    ]

(* ---------- eval tests ---------- *)
let eval_tests =
  "testing eval" >:::
    [
      "application" >:: (fun _ ->
        let expected = Ok (VNum 4) in
        let actual = eval (App (Fun ("x", Bop (Add, Var "x", Num 1)), Num 3)) in
        assert_equal expected actual);
    ]

(* ---------- interp tests ---------- *)
let interp_tests =
  "interp tests" >:::
    [
      "variable" >:: (fun _ ->
        let expected = Ok (VBool true) in
        let actual = interp "let x = true || false in x && true" in
        assert_equal expected actual);
    ]

(* ---------- recursion tests (extra credit) ---------- *)
let recursion_tests =
  "recursion tests" >:::
    [
      "fact 5 = 120" >:: (fun _ ->
        let prog =
          "let rec fact = fun n -> \
             if n <= 0 then 1 else n * fact (n - 1) \
           in fact 5"
        in
        assert_equal (Ok (VNum 120)) (interp prog));

      "fib 6 = 8" >:: (fun _ ->
        let prog =
          "let rec fib = fun n -> \
             if n <= 1 then n else fib (n - 1) + fib (n - 2) \
           in fib 6"
        in
        assert_equal (Ok (VNum 8)) (interp prog));

      "outer shadowing" >:: (fun _ ->
        let prog =
          "let f = 999 in \
           let rec f = fun n -> if n = 0 then 0 else 1 + f (n - 1) \
           in f 3"
        in
        assert_equal (Ok (VNum 3)) (interp prog));

      "rec on non-function is no-op" >:: (fun _ ->
        let prog = "let rec x = 42 in x" in
        assert_equal (Ok (VNum 42)) (interp prog));

      "param name same as internal self name" >:: (fun _ ->
        let prog =
          "let rec f = fun __self -> \
             if __self <= 1 then 1 else __self * f (__self - 1) \
           in f 4"
        in
        assert_equal (Ok (VNum 24)) (interp prog));
    ]
;;

(* ---------- extra desugar tests ---------- *)
let desugar_tests =
  "desugar tests" >:::
    [
      "and short-circuit" >:: (fun _ ->
        let p = "false && (1 / 0 = 0)" in
        assert_equal (Ok (VBool false)) (interp p));

      "or short-circuit" >:: (fun _ ->
        let p = "true || (1 / 0 = 0)" in
        assert_equal (Ok (VBool true)) (interp p));

      "lte via lt/eq" >:: (fun _ ->
        assert_equal (Ok (VBool true)) (interp "3 <= 3");
        assert_equal (Ok (VBool true)) (interp "2 <= 3");
        assert_equal (Ok (VBool false)) (interp "4 <= 3"));

      "gte via gt/eq" >:: (fun _ ->
        assert_equal (Ok (VBool true)) (interp "3 >= 3");
        assert_equal (Ok (VBool true)) (interp "4 >= 3");
        assert_equal (Ok (VBool false)) (interp "2 >= 3"));

      "neq via if" >:: (fun _ ->
        assert_equal (Ok (VBool true)) (interp "4 <> 5");
        assert_equal (Ok (VBool false)) (interp "4 <> 4"));

      "desugar precedence keeps and>or" >:: (fun _ ->
        let p = "true || false && false" in
        assert_equal (Ok (VBool true)) (interp p));
    ]
;;

let tests =
  "interp1 test suite" >:::
    [
      parse_tests;
      subst_tests;
      eval_tests;
      interp_tests;
      recursion_tests;
      desugar_tests;
    ]

let _ = run_test_tt_main tests