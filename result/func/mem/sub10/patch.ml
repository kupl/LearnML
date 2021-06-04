type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool =
  match tree with
  | Empty -> false
  | Node (a, bt1, bt2) -> if a != n then mem n bt1 || mem n bt2 else true
