type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	match tree with
	| Empty -> false
	| Node(key, lc, rc) -> if key=n then true
				else (mem n lc) || (mem n rc);;
