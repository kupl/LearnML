(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
|Node(i,x,y) -> Node(i, mirror y, mirror x)
|_ -> Empty
