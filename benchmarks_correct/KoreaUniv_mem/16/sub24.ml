type btree =
	| Empty
	| Node of int * btree * btree

let rec mem n tree = 
match tree with
|Empty -> false
|Node (x,l,r) -> if n=x then true
 else (mem n l)||(mem n r);;
