type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	| Empty -> false
	| Node (a, l1, l2) -> if a = n then true else mem n l1||mem n l2 
