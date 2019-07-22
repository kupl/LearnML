type btree =
	| Empty
	| Node of int * btree * btree

let rec mem n tree = 
	match tree with 
		|Empty -> false 
		|Node(a,b,c) -> if a=n then true 
						else if (mem n b) then true
						else (mem n c);; 