type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	|Empty -> false
	|Node (k, bt1, bt2) ->
		if n=k then true
		else if (mem n bt1)=true then true
		else mem n bt2
