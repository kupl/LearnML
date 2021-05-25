type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node (i, left, right) -> n = i || mem n left || mem n right
