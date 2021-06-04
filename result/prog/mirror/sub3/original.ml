type btree = Empty | Node of (int * btree * btree)

let rec mirror (t : btree) : btree =
  match t with Empty -> Empty | Node (pr, lc, rc) -> Node (pr, rc, lc)
