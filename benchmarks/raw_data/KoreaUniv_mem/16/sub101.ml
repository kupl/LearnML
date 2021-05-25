type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->  (* TODO *)
match tree with
| Empty -> false
| Node (a, tree1, tree2) -> 
((a=n) || (mem n tree1) || (mem n tree2))
