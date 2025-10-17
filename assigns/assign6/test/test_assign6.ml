open OUnit2

let tests =
  let t s v = fun _ -> assert_equal (Assign6.interp ~print:false s) v in
  "interpreter test suite" >:::
  [
    "int" >:: t "3" (Some (IntV 3));
    "true" >:: t "True" (Some (BoolV true));
    "false" >:: t "False" (Some (BoolV false));
    "add" >:: t "(+ 2 3)" (Some (IntV 5));
    "sub" >:: t "(- 2 3)" (Some (IntV (-1)));
    "mul" >:: t "(* 2 3)" (Some (IntV 6));
    "div" >:: t "(/ 2 3)" (Some (IntV 0));
    "lte" >:: t "(<= 2 3)" (Some (BoolV true));
    "if" >:: t "(If (<= 2 3) 1 2)" (Some (IntV 1));
    "let" >:: t "(Let x 1 (+ x x))" (Some (IntV 2));
    "nested let" >:: t "(Let x 1 (Let y 2 (+ x y)))" (Some (IntV 3));
    "ill-typed add" >:: t "(+ 1 (<= 2 3))" None;
    "ill-typed let" >:: t "(Let x (<= 2 3) (+ 1 x))" None;
    "ill-typed if" >:: t "(Let x 1 (If x x x))" None;
    "larger example" >::
    t
      "
      (Let x 2
      (Let y (+ x x)
      (Let z (- ( * y x) 1)
      (Let q (If (<= z 0) (- 0 z) z)
      (/ q 3)))))
      "
      (Some (IntV 2));
  ]

let _ = run_test_tt_main tests
