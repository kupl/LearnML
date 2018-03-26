type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
	match tree with
	| Empty -> false
	| Node(x, left, right) -> if( n=x || (mem n left) || (mem n right) ) then true else false
