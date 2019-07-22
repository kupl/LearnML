type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
match tree with
	| Empty -> false
	| Node ( m, tree_L, tree_R ) -> 
		if n = m then true
		else ( mem n tree_L ) || ( mem n tree_R );;
