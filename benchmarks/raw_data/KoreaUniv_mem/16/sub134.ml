type btree =
	| Empty
	| Node of int * btree * btree;;

let rec mem : int -> btree -> bool
= fun n tree -> 
	match tree with
	| Empty -> false
	| Node(x, node1, node2) ->
		if x = n then true else (mem n node1) || (mem n node2);;