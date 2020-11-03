type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
	Empty -> false
	|Node(a,left,right) -> (mem n left)||(a = n)||(mem n right) (* TODO *)
