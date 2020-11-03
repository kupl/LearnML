type btree = Empty | Node of int * btree * btree

let rec mem a t =
	match t with
	| Empty -> false
	| Node(q, p, r) -> 
		if q = a then true
		else mem a p || mem a r