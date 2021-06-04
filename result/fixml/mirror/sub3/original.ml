type btree = Empty | Node of (int * btree * btree)

let rec mirror : btree -> btree =
 fun t -> match t with Empty -> Empty | Node (pr, lc, rc) -> Node (pr, rc, lc)
