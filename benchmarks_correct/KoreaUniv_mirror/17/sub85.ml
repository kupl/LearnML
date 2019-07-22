(*problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree->btree = fun t ->
match t with 
	|Empty -> Empty
	|Node (root, Empty, Empty) -> Node (root, Empty, Empty)
	|Node (root, left, Empty) -> Node (root, Empty, mirror left)
	|Node (root, Empty, right) -> Node (root, mirror right, Empty)
	|Node (root,left,right) -> Node (root, mirror right, mirror left)
;;
