type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with | Empty -> false | Node(a, Empty, Empty) -> if a==n then true else false | Node(a, b, Empty) -> if a==n then true else if mem n b then true else false | Node(a, Empty, c) -> if a==n then true else if mem n c then true else false | Node(a, b, c) -> if a==n then true else if mem n b || mem n c then true else false;;
