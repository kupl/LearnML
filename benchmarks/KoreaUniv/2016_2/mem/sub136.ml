type btree = | Empty | Node of int * btree * btree
let rec mem : int -> btree -> bool
	= fun n tree -> match tree with
		| Empty -> false
		| Node (m,btreex,btreey) -> if m = n then true
			else if btreex=Empty then mem n btreey
			else if btreey=Empty then mem n btreex
			else mem n btreex || mem n btreey;;
