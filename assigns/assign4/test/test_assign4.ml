open Assign4

let testing = true

let run cases b = if b then cases () else []

let sexpr_of_tokens_tests () =
  [
    assert (sexpr_of_tokens [Atom "a"] = Some (Atom "a", []));
    assert (sexpr_of_tokens [Atom "a"; Atom "b"] = Some (Atom "a", [Atom "b"]));
    assert (sexpr_of_tokens [Lparen; Atom "a"; Rparen; Atom "b"] = Some (List [Atom "a"], [Atom "b"]));
    assert (sexpr_of_tokens [Lparen; Atom "a"; Atom "b"; Rparen] = Some (List [Atom "a"; Atom "b"], []));
    assert (sexpr_of_tokens [Lparen; Atom "a"; Lparen; Rparen; Atom "b"; Rparen] = Some (List [Atom "a"; List []; Atom "b"], []));
    assert (sexpr_of_tokens [Lparen; Atom "a"; Lparen; Rparen; Atom "b"; Rparen; Lparen] = Some (List [Atom "a"; List []; Atom "b"], [Lparen]));
  ]

let sexprs_of_tokens_tests () =
  [
    assert (sexprs_of_tokens [Atom "a"] = ([Atom "a"], []));
    assert (sexprs_of_tokens [Atom "a"; Atom "b"] = ([Atom "a"; Atom "b"], []));
    assert (sexprs_of_tokens [Lparen; Atom "a"; Rparen; Atom "b"] = ([List [Atom "a"]; Atom "b"], []));
    assert (sexprs_of_tokens [Lparen; Atom "a"; Atom "b"; Rparen] = ([List [Atom "a"; Atom "b"]], []));
    assert (sexprs_of_tokens [Lparen; Atom "a"; Lparen; Rparen; Atom "b"; Rparen] = ([List [Atom "a"; List []; Atom "b"]], []));
    assert (sexprs_of_tokens [Lparen; Atom "a"; Lparen; Rparen; Atom "b"; Rparen; Lparen] = ([List [Atom "a"; List []; Atom "b"]], [Lparen]));
  ]

let parse_sexpr_tests () =
  [
    assert (parse_sexpr "((a b) (c d))" = Some (List [List [Atom "a"; Atom "b"]; List [Atom "c"; Atom "d"]]));
    assert (parse_sexpr "(a b) (c d))" = None);
  ]

let expr_of_sexpr_tests () =
  [
    assert (expr_of_sexpr (List [Atom "+"; List [Atom "-"; Atom "1"; Atom "2"]; Atom "3"])
            = Some (Add (Sub (Int 1, Int 2), Int 3)));
    assert (expr_of_sexpr (List [Atom "+"; List [Atom "-"; Atom "1"; Atom "2"]; Atom "3"; Atom "4"])
            = None);
    assert (expr_of_sexpr (List [Atom "+"; List [Atom "-"; Atom "1"; Atom "2"]; Atom "a"])
            = None);
  ]

let interp_tests () =
  [
    assert (interp "(+ (* 2 3) (- 4 5))" = Some (5));
    assert (interp "(+ (+ (+ 1 2) 3) 4)" = Some 10);
    assert (interp "((+ (+ (+ 1 2) 3) 4)" = None);
  ]

let _run_tests =
  if not testing then [] else
    [
      run sexpr_of_tokens_tests true;
      run sexprs_of_tokens_tests true;
      run parse_sexpr_tests true;
      run expr_of_sexpr_tests true;
      run interp_tests true;
    ]
