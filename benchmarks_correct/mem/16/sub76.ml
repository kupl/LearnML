type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
match tree with
	Node (num, t1, t2) -> if n = num then true else if (mem n t1) || (mem n t2) then true else false
|	Empty -> false
