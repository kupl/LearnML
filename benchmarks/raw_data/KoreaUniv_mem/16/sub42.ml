type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
 match tree with
| Empty -> false
| Node (m, Empty, Empty) -> n=m
| Node (m, a, Empty) -> n=m || mem n a
| Node (m, Empty, b) -> n=m || mem n b
| Node (m, a, b) -> n=m || mem n a || mem n b
