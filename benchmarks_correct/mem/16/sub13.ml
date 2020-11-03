type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
Empty -> false |
Node (k, l, m) -> if k == n then true
else (if mem n l then true
else (if mem n m then true else false))