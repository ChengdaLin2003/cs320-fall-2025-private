open Interp2
open OUnit2

(******************************************************************
 * desugar tests
 ******************************************************************)

let desugar_tests =
  "desugar test suite" >:::
    [
      "basic test" >:: (fun _ ->
        let prog =
          Option.get
            (parse
               "let id (x : int) : int = x
                let y : int = 5")
        in
        let expected =
          Let
            {
              is_rec = false;
              name = "id";
              ty = FunTy (IntTy, IntTy);
              binding = Fun ("x", IntTy, Var "x");
              body =
                Let
                  {
                    is_rec = false;
                    name = "y";
                    ty = IntTy;
                    binding = Num 5;
                    body = Var "y";
                  };
            }
        in
        let actual = desugar prog in
        assert_equal expected actual);
      (* 可以按需要继续加 desugar 测试 *)
    ]

(******************************************************************
 * type_of tests
 ******************************************************************)

let type_of_tests =
  "type_of test suite" >:::
    [
      "basic fun type" >:: (fun _ ->
        let expr = Fun ("x", BoolTy, Num 5) in
        let expected = Ok (FunTy (BoolTy, IntTy)) in
        let actual = type_of expr in
        assert_equal expected actual
      );
      (* 可以按需要继续加 type_of 测试 *)
    ]

(******************************************************************
 * eval tests — 主要是递归函数
 ******************************************************************)

let eval_tests =
  "eval test suite" >:::
    [
      "recursive function (factorial)" >:: (fun _ ->
        (* let rec fact (n:int) : int =
             if n = 0 then 1 else n * fact (n - 1)
           in fact 5 *)
        let fact_expr =
          Let
            {
              is_rec = true;
              name = "fact";
              ty = FunTy (IntTy, IntTy);
              binding =
                Fun
                  ( "n",
                    IntTy,
                    If
                      ( Bop (Eq, Var "n", Num 0),
                        Num 1,
                        Bop
                          ( Mul,
                            Var "n",
                            App
                              ( Var "fact",
                                Bop (Sub, Var "n", Num 1) ) ) ) );
              body = App (Var "fact", Num 5);
            }
        in
        let expected = VNum 120 in
        let actual = eval fact_expr in
        assert_equal expected actual);
    ]

(******************************************************************
 * interp tests — 对应 autograder 的几个点
 ******************************************************************)

let interp_tests =
  "interp test suite" >:::
    [
      "interp simple recursive fact" >:: (fun _ ->
        (* 程序：
             let rec fact (n : int) : int =
               if n = 0 then 1 else n * fact (n - 1)
             let res : int = fact 5
           结果是最后一个 toplet 的名字 res 的值 *)
        let prog_str =
          "let rec fact (n : int) : int =
             if n = 0 then 1 else n * fact (n - 1)
           let res : int = fact 5"
        in
        let expected = Ok (VNum 120) in
        let actual = interp prog_str in
        assert_equal expected actual);

      "interp fibonacci" >:: (fun _ ->
        (* fib(0)=0, fib(1)=1, fib(7)=13 *)
        let prog_str =
          "let rec fib (n : int) : int =
             if n < 2 then n else fib (n - 1) + fib (n - 2)
           let result : int = fib 7"
        in
        let expected = Ok (VNum 13) in
        let actual = interp prog_str in
        assert_equal expected actual);

      "interp prime" >:: (fun _ ->
        (* 简单素数测试：is_prime 17 = true, 21 = false *)
        let prog_str_true =
          "
          let rec is_prime (n : int) : bool =
            let rec check (d : int) : bool =
              if d * d > n then true
              else if n mod d = 0 then false
              else check (d + 1)
            in
            if n <= 1 then false else check 2
          let result : bool = is_prime 17
          "
        in
        let prog_str_false =
          "
          let rec is_prime (n : int) : bool =
            let rec check (d : int) : bool =
              if d * d > n then true
              else if n mod d = 0 then false
              else check (d + 1)
            in
            if n <= 1 then false else check 2
          let result : bool = is_prime 21
          "
        in
        let expected_true = Ok (VBool true) in
        let expected_false = Ok (VBool false) in
        let actual_true = interp prog_str_true in
        let actual_false = interp prog_str_false in
        assert_equal expected_true actual_true;
        assert_equal expected_false actual_false);
    ]

(******************************************************************
 * test suite aggregation
 ******************************************************************)

let tests =
  "interp2 test suite" >:::
    [
      desugar_tests;
      type_of_tests;
      eval_tests;
      interp_tests;
    ]

let _ = run_test_tt_main tests