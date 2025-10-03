open Assign5

let testing = true

let run cases b = if b then cases () else []

let hd_opt l =
  match l with
  | [] -> None
  | h :: _ -> Some h

let basic_tests () =
  [
    assert (curry3 (fun (x, y, z) -> x + y + z) 1 2 3 = 6);
    assert (uncurry3 (fun x y z -> x + y + z) (1, 2, 3) = 6);
    assert (filter_map hd_opt [[1;2;3];[];[];[2;3;4];[];[5]] = [1;2;5]);
  ]

let t =
  Node
    ( 1
    , [ Node
        ( 2
        , [ Node (3, []);
            Node (4, []);
            Node (5, [Node (6, [])]);
          ]
        );
        Node (7, []);
        Node (8, [Node (9, [])]);
      ]
    )

let t2 =
  Node
    ( 10
    , [ Node
        ( 20
        , [ Node (30, []);
            Node (40, []);
            Node (50, [Node (60, [])]);
          ]
        );
        Node (70, []);
        Node (80, [Node (90, [])]);
      ]
    )

let t3 =
  Node
    ( 1
    , [ Node
        ( 2
        , [ Node (3, []);
          ]
        );
        Node (8, [Node (9, [])]);
      ]
    )

let rtree_tests () =
  [
    assert (map_rtree (( * ) 10) t = t2);
    assert (filter_rtree (fun x -> x < 4 || x > 7) t = Some t3);
    assert (filter_rtree (fun x -> x < 1) t = None);
  ]

let e1 = Bop (Add, Int 1, Bop(Mul, Var "x", Int 3))
let e1_str = "(1 + (x * 3))"
let e2 = Bop(Sub, Int 4, Call ("foo", [Int 1; Int 2]))
let e2_str = "(4 - foo(1, 2))"

let string_of_expr_tests () =
  [
    assert (string_of_expr e1 = e1_str);
    assert (string_of_expr e2 = e2_str);
  ]

let s1 = FunDef ("foo", ["x"; "y"], [
    Assign ("z", e1);
    Print (Var "z");
    Return e2;
  ])
let s1_str =
"def foo(x, y):
    z = (1 + (x * 3))
    print(z)
    return (4 - foo(1, 2))"

let s2 = FunDef ("bar", ["x"; "y"], [
    FunDef("baz", [], [
        Return (Var "x")
      ]);
    Return (Call ("baz", []));
  ])
let s2_str =
"def bar(x, y):
    def baz():
        return x
    return baz()"

let string_of_stmt_tests ()=
  [
    assert (string_of_stmt s1 = s1_str);
    assert (string_of_stmt s2 = s2_str);
  ]

let p = [
  s1;
  s2;
  Print (Bop(Div, Call ("foo", [Int 1; Int 2]), Call ("bar", [Int 3; Int 4])));
]

let p_str =
"def foo(x, y):
    z = (1 + (x * 3))
    print(z)
    return (4 - foo(1, 2))
def bar(x, y):
    def baz():
        return x
    return baz()
print((foo(1, 2) / bar(3, 4)))"

let string_of_prog_tests () =
  [
    assert (string_of_prog p = p_str);
  ]


let _run_tests =
  if not testing then [] else
    [
      run basic_tests true;
      run rtree_tests true;
      run string_of_expr_tests true;
      run string_of_stmt_tests true;
      run string_of_prog_tests true;
    ]
