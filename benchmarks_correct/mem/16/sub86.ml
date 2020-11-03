type btree =
        | Empty
        | Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
	match tree with
	|Empty -> false
	|Node(p,l,r) -> if p = n then true 
			else (mem n l) ||  (mem n r)
