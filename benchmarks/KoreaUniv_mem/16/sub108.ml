type btree = 
	| Empty
	| Node of int * btree * btree

let rec mem n tree =
match tree with 
| Empty -> false
| Node (a, tree1, tree2) -> if (a = n || mem n tree1 || mem n tree2) then true
else false
