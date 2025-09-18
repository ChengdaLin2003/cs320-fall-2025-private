open Assign2

let testing = true

let run cases b = if b then cases () else []

let drop_leading_tests () =
  [
    assert (drop_leading 1 [1;2;3] = [2;3]);
    assert (drop_leading 1 [1;1;2;3] = [2;3]);
    assert (drop_leading 1 [2;3] = [2;3]);
  ]

let drop_trailing_tests () =
  [
    assert (drop_trailing 1 [2;3;1] = [2;3]);
    assert (drop_trailing 1 [2;3;1;1] = [2;3]);
    assert (drop_trailing 1 [2;3] = [2;3]);
  ]

let split_on_char_tests () =
  [
    assert (split_on_char ' ' "a b c" = ["a";"b";"c"]);
    assert (split_on_char ' ' " a b c" = ["";"a";"b";"c"]);
    assert (split_on_char ' ' " a  b c" = ["";"a";"";"b";"c"]);
    assert (split_on_char '/' "/a//b/c/" = ["";"a";"";"b";"c";""]);
    assert (split_on_char ' ' "1/2 2/3 3/4" = ["1/2";"2/3";"3/4"]);
  ]

let parse_fractran_tests () =
  let q m n = Q.make Z.(~$ m) Z.(~$ n) in
  [
    assert (parse_fractran "1/2 2/3 3/4" = [q 1 2; q 2 3; q 3 4]);
    assert (parse_fractran "-1/2 2/3 3/4" = [q (-1) 2; q 2 3; q 3 4]);
    assert (parse_fractran "-1/-2 2/3 3/4" = [q (-1) (-2); q 2 3; q 3 4]);
  ]

let eval_fractran_tests () =
  let q m n = Q.make Z.(~$ m) Z.(~$ n) in
  [
    assert (eval_fractran [q 3 2] Z.(~$ 2 ** 10) = Z.(~$ 3 ** 10));
    assert (eval_fractran [q 3 2; q 5 3] Z.(~$ 2 ** 10) = Z.(~$ 5 ** 10));
    assert (eval_fractran [q 5 6; q 1 2; q 1 3] Z.(~$ 2 ** 4 * ~$ 3 ** 6) = Z.(~$ 5 ** 4))
  ]

let _run_tests =
  if not testing then [] else
    [
      run drop_leading_tests true;
      run drop_trailing_tests true;
      run split_on_char_tests true;
      run parse_fractran_tests true;
      run eval_fractran_tests true;
    ]
