type btree = Empty | Node of (int * btree * btree)

let rec mem (n : int) (tree : btree) : bool = true
