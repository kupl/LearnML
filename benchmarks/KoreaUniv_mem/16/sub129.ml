type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> (* TODO *)
match tree with
| Empty -> false
| Node(m,t1,t2) -> if m=n then true else
					mem n t1 || mem n t2