type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
	| Empty -> false
	| Node (x, leftBranch, rightBranch) -> (x = n || mem n leftBranch || mem n rightBranch);;