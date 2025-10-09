
type 'a tree23 =
  | Leaf2
  | Node2 of 'a * 'a tree32 * 'a tree32
and 'a tree32 =
  | Leaf3
  | Node3 of 'a * 'a tree23 * 'a tree23 * 'a tree23

let rec count (t : 'a tree23) : int =
  match t with
  | Leaf2 -> 0
  | Node2 (_, t1, t2) -> count3 t1 + count3 t2 + 1
and count3 t =
  match t with
  | Leaf3 -> 0
  | Node3 (_, t1, t2, t3) -> count t1 + count t2 + count t3 + 1

let rec sum (t : int tree23) : int =
  match t with
  | Leaf2 -> 0
  | Node2 (n, t1, t2) -> sum3 t1 + sum3 t2 + n
and sum3 t =
  match t with
  | Leaf3 -> 0
  | Node3 (n, t1, t2, t3) -> sum t1 + sum t2 + sum t3 + n
