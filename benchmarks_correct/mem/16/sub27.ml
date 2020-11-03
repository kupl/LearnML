type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match  tree with
Empty -> false
|Node (a , ltree , rtree) -> if a=n then true else mem n ltree || mem n rtree;; (* TODO *)
