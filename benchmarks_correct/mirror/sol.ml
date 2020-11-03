type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> 
	match t with
	| Empty -> Empty
	| Node (n, l, r) -> Node (n, mirror r, mirror l)