type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with

Empty -> false
|Node (a, l, r) -> if n = a then true
else mem n l|| mem n r;;
