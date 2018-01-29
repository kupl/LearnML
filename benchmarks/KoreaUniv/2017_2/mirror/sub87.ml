
(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
match t with 
|Empty -> Empty
|Node(int, Empty, Empty) -> Node(int, Empty, Empty)
|Node(int, left, right) -> Node(int, mirror right, mirror left)

