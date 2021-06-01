type btree =
	| Empty
	| Node of int * btree * btree

let rec mem n tree =
	match tree with
	| Empty -> false
	| Node(c, left, right) ->
		if n = c then true
		else mem n left || mem n right;;