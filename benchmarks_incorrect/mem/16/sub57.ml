type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
	let rec leaves = function
	    | Empty -> []
	    | Node(c, Empty, Empty) -> [c]
	    | Node(_, l, r) -> leaves l @ leaves r in
	let rec search a = function
		| [] -> false
		| hd::tl -> if hd = a then true else search a tl in
	search n (leaves tree)
