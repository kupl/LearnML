type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
	| Empty -> false
	| Node (m, l, r) -> if m = n then true else mem n l || mem n r
