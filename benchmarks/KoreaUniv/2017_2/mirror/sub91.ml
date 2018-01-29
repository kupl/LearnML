(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
match t with
|Empty -> Empty
|Node (i, n1, n2) -> Node (i, mirror n2, mirror n1)
