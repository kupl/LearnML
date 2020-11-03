type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
									| Empty -> false
									| Node(m, btree1, btree2) -> if m = n then true
																									else mem n btree1 || mem n btree2
