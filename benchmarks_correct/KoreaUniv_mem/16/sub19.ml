type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  (* TODO *)
match tree with
|Node (a, b, c) -> if n = a then true else mem n b || mem n c
|Empty -> false
