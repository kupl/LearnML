(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with|Empty->Empty|Node(root,left,right)->Node(root,mirror(right),mirror(left));;