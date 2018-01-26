(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
|Empty -> Empty
|Node (i, b1, b2) -> Node (i, mirror b2, mirror b1)
