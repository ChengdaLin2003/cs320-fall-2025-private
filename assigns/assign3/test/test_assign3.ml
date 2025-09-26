open Assign3

let testing = true

let run cases b = if b then cases () else []

let load_tests () =
  [
    assert (load [] = []);
    assert (load [1;0;2] = [(0, 1); (2, 2)]);
    assert (load [1;1] = [(0, 1); (1, 1)]);
    assert (load [1;2;3] = [(0, 1); (1, 2); (2, 3)]);
  ]

let lookup_tests () =
  [
    assert (lookup [] (-230) = 0);
    assert (lookup (load [1;2;3]) 1 = 2);
  ]

let incr_tests () =
  [
    assert (incr (load [1;2;3]) 2 = load [1;2;4]);
    assert (incr [] 5 = [(5, 1)]);
    assert (incr (load [1;0;3]) 1 = load [1;1;3]);
    assert (incr [(0, 4)] (-1) = [(-1, 1); (0, 4)]);
  ]

let zero_tests () =
  [
    assert (zero [(5, 5)] 5 = []);
    assert (zero (load [1;2;3]) 5 = load [1;2;3]);
    assert (zero (load [1;2;3]) 1 = [(0, 1);(2, 3)]);
  ]

let transfer_tests () =
  [
    assert (transfer [] 4 10 = []);
    assert (transfer [(5, 1)] 10 5 = []);
    assert (transfer (load [1;2;3]) 1 5 = (load [1;2;3;0;0;2]));
  ]

let parse_urm_tests () =
  [
    assert (parse_urm ["J"; "9"; "-1"; "5"] = [[3; 9; -1; 5]]);
    assert (parse_urm ["I"; "0"; "Z"; "1"; "T"; "2"; "3"; "J"; "4"; "5"; "6"]
            = [[1;0]; [0;1]; [2;2;3]; [3;4;5;6]]);
  ]

let eval_urm_tests () =
  [
    assert (eval_urm [[1;1]; [1;1]] [] = [(1, 2)]);
    assert (eval_urm [[1;1]; [1;1]; [0; 1]] [] = []);
    assert (eval_urm [[3; 0; 1; 2]; [1; 0]; [1; 1]] [(0, 5); (1, 5)] = [(0, 5); (1, 6)]);
    assert (eval_urm [[3; 0; 1; 2]; [2; 0; 2]; [1; 1]] [(0, 5); (1, 2)] = [(0, 5); (1, 3); (2, 5)]);
  ]

let _run_tests =
  if not testing then [] else
    [
      run load_tests true;
      run lookup_tests true;
      run incr_tests true;
      run zero_tests true;
      run transfer_tests true;
      run parse_urm_tests true;
      run eval_urm_tests true;
    ]
