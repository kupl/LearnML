type btree = Empty | Node of (int * btree * btree)

let rec mirror (t : btree) : btree =
  match t with Node (x, l, r) -> Node (x, r, l) | Empty -> Empty
