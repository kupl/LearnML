type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
|Empty -> false
|Node(x,n1,n2) -> if(n=x) then true else mem n n1||mem n n2;;
