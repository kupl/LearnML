type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  (* TODO *)
	match tree with
	| Empty -> false
	| Node (rt, ltree, rtree) ->
		if rt == n then true
		else if mem n ltree = true then true
		else if mem n rtree = true then true
		else false
