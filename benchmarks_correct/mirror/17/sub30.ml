(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec  mirror : btree->btree
=fun t -> match t with
	|Empty -> Empty
	|Node (a, Empty, Empty) -> t
	|Node (a, Empty, right) -> Node(a, mirror (right), Empty)
	|Node (a, left, Empty) -> Node(a, Empty, mirror(left)) 
	|Node (a, left, right) -> Node (a, mirror (right), mirror (left));;
