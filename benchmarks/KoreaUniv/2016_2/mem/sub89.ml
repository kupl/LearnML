type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
match tree with
| Empty -> false
| Node (num,t1,t2) -> if num = n then true else ((mem n t1) || (mem n t2));;
