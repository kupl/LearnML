type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	| Empty ->	false
	| Node (n1, ltree, rtree) -> if n1 = n then true else (mem n ltree) || (mem n rtree)