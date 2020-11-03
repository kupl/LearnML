type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	| Empty -> false
	| Node (a,t1,t2) -> if a = n then true else (mem n t1) || (mem n t2)
