type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
| Empty -> false
| Node (k, leftchild, rightchild) -> if k=n then true else (mem n leftchild) || (mem n rightchild)
