type btree = Empty | Node of (int * btree * btree)

let rec mirror : btree -> btree =
 fun f -> match f with Empty -> Empty | Node (a, b, c) -> Node (a, c, b)
