(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->

match t with
| Empty -> Empty
| Node(num, left, right) -> Node(num, mirror right, mirror left)

