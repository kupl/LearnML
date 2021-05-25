type btree =
	| Empty
	| Node of int * btree * btree

let rec mem n bt =
match bt with
|Empty -> false
|Node (x, b1, b2) -> if x == n then true else (mem n b1 || mem n b2)
