
let example : int Lab05.tree23 =
  Node2
    ( 1
    , Node3
        ( 2
        , Node2 (3, Leaf3, Leaf3)
        , Node2 (4, Leaf3, Leaf3)
        , Node2 (5, Leaf3, Leaf3)
        )
    , Node3
        ( 6
        , Node2 (7, Leaf3, Leaf3)
        , Leaf2
        , Leaf2
        )
    )

let _ = assert (Lab05.count example = 7)
let _ = assert (Lab05.sum example = 28)
