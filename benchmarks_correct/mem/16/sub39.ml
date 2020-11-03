type btree =
	| Empty
	| Node of int * btree * btree

let rec mem x tree = 
	match tree with
	| Empty -> false
	| Node (k, l, r) -> if x = k then true
											else (mem x r) || (mem x l)
