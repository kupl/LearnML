type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node(a, left, right) -> if a = n then true
else if mem n left = true then true
else if mem n right = true then true
else false
