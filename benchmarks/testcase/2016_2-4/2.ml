type btree =
	| Empty
	| Node of int * btree * btree 

let rec f : int -> btree -> bool
= fun n tree -> 
	match tree with
	|Empty -> false
	|Node (num, bt1, bt2) -> if n = num then true else (f num bt1 || f num bt2);;
