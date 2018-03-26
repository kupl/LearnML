type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with | Empty -> false 
							| Node(a,node_left,node_right) -> if a=n then true else (mem n node_left) || (mem n node_right)
