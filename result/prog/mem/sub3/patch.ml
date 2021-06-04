type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (num, bt1, bt2) -> if n = num then true else mem n bt1 || mem n bt2
