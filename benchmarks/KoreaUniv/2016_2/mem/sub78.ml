type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	| Empty ->	false
	| Node (n1, _, _) when n1 = n ->	true
	| Node (_, ltree, rtree) -> (mem n ltree) || (mem n rtree)
