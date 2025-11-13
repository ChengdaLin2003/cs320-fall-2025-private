open OUnit2
open Utils
open Interp2

(* 把 expr 打印出来，方便调试 *)
let rec string_of_expr = function
  | Unit -> "Unit"
  | True -> "True"
  | False -> "False"
  | Num n -> "Num " ^ string_of_int n
  | Var x -> "Var " ^ x
  | Let (x, ty, e1, e2) ->
      "Let(" ^ x ^ " : " ^ string_of_ty ty ^ ", "
      ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | LetRec (f, ty, e1, e2) ->
      "LetRec(" ^ f ^ " : " ^ string_of_ty ty ^ ", "
      ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | If (c, t, f) ->
      "If(" ^ string_of_expr c ^ ", "
      ^ string_of_expr t ^ ", " ^ string_of_expr f ^ ")"
  | Fun (x, ty, e) ->
      "Fun(" ^ x ^ " : " ^ string_of_ty ty ^ ", " ^ string_of_expr e ^ ")"
  | App (e1, e2) ->
      "App(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Bop (_op, e1, e2) ->
      "Bop(…, " ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Assert e ->
      "Assert(" ^ string_of_expr e ^ ")"

let assert_expr_equal ?msg expected actual =
  if expected = actual then ()
  else
    let msg =
      match msg with
      | Some m -> m
      | None ->
          "expected: " ^ string_of_expr expected ^ "\nactual:   "
          ^ string_of_expr actual
    in
    assert_failure msg

(* ---------- desugar tests ---------- *)

let desugar_tests =
  "desugar tests" >:::
    [
      (* 空 program → Unit *)
      "empty prog" >:: (fun _ ->
        let prog : prog = [] in
        let got = desugar prog in
        assert_expr_equal Unit got);

      (* 单个非递归顶层：
         let f (x:int) : int = x
         我们的 desugar 只保留 ann（= TInt），不会自己做 t1->t2 箭头 *)
      "single non-rec toplet" >:: (fun _ ->
        let top : toplet =
          {
            is_rec = false;
            name   = "f";
            args   = [ ("x", TInt) ];
            ann    = TInt;
            body   = Var "x";
          }
        in
        let prog = [ top ] in
        let got  = desugar prog in
        let expected =
          Let ("f",
               (* 类型字段就是 ann = TInt *)
               TInt,
               Fun ("x", TInt, Var "x"),
               Var "f")
        in
        assert_expr_equal expected got);

      (* 两个顶层定义：
         let g (y:int) : int = y + 1
         let rec f (x:int) : int = x
         desugar 后类型字段仍然是 ann = TInt，而不是 (int -> int) *)
      "two toplets nested" >:: (fun _ ->
        let top_g : toplet =
          {
            is_rec = false;
            name   = "g";
            args   = [ ("y", TInt) ];
            ann    = TInt;
            body   = Bop (Add, Var "y", Num 1);
          }
        in
        let top_f : toplet =
          {
            is_rec = true;
            name   = "f";
            args   = [ ("x", TInt) ];
            ann    = TInt;
            body   = Var "x";
          }
        in
        let prog : prog = [ top_g; top_f ] in
        let got = desugar prog in
        let expected =
          Let ("g",
               TInt,
               Fun ("y", TInt, Bop (Add, Var "y", Num 1)),
               LetRec ("f",
                       TInt,
                       Fun ("x", TInt, Var "x"),
                       Var "f"))
        in
        assert_expr_equal expected got);
    ]

let () = run_test_tt_main desugar_tests