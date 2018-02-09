type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with 
| Empty -> false
| Node (x,t1,t2) -> n = x || mem n t1 || mem n t2
