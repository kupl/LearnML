type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node (a, b1, b2) ->
	if a = n then true
	else mem n b1 || mem n b2 
