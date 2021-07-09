type btree = Empty | Node of (int * btree * btree)

let rec mirror (f : btree) : btree =
  match f with Empty -> Empty | Node (a, b, c) -> Node (a, mirror c, mirror b)
