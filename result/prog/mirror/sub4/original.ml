type btree = Empty | Node of (int * btree * btree)

let rec mirror (t : btree) : btree =
  match t with Empty -> Empty | Node (a, b, c) -> Node (a, c, b)
