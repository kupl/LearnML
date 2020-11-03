type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> (* TODO *)
	match tree with
	|Empty -> false
	|Node(mid, left , right) -> 

	if ( n=mid ) then true 
	else ( mem n left || mem n right )