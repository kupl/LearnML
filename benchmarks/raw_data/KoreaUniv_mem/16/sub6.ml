type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
match tree with
| Empty -> false
| Node(y, l, r) ->
	if n = y then true
	else (mem n l) || (mem n r)
