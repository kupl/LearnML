type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  (* TODO *)
match tree with
|Node (a, b, c) when n=a -> true
|Node (a, b, c) -> mem n b || mem n c
|Empty -> false
