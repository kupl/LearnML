type btree = Empty | Node of (int * btree * btree)

let rec mem (x : int) (btree : btree) : bool =
  match btree with
  | Empty -> false
  | Node (y, left, right) -> if y != x then mem x left || mem x right else true
