type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
	match tree with
	| Empty -> false
	| Node(b,c,d) -> if n = b then true 
					 else if mem n c = true || mem n d = true then true
					 else false
