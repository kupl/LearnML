(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
		| Node (x, left, right) -> Node (x, mirror right, mirror left)
		| Empty -> Empty
