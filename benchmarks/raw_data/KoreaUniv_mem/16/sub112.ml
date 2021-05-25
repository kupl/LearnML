type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
	= fun n tree ->
		match tree with
		| Empty -> false
		| Node (num, tree1, tree2) ->
			if num = n then true
			else mem n tree1 || mem n tree2;;
