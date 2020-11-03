type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with
| Empty -> false
| Node(p,q,r) -> if (p=n) then true else if (mem n q) then true else if (mem n r) then true else false;; 