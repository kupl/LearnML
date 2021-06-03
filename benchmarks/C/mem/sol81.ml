type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
	match tree with
	| Node (i, t1, t2) -> if i = n then true
										 else (mem n t1) || (mem n t2)
	| Empty -> false
