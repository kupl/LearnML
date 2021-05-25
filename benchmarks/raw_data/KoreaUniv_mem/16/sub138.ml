type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
	match tree with
	|	Empty -> false
	|	Node (v, l, r) -> (n = v) || (mem n l) || (mem n r)
