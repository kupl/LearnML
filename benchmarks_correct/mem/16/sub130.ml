type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
        | Empty -> false
        | Node (a, b, c) -> if a = n then true
        else false || mem n b || mem n c;;