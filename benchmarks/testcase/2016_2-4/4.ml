type btree =
	| Empty
	| Node of int * btree * btree

let rec f : int -> btree -> bool
= fun n tree -> match tree with
		Empty -> false
	|Node(a, b, c) -> 
		if a == n then true
		else if a < n then f n b
		else f n c;;