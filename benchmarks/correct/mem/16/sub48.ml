type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
match tree with
|Empty ->false
|Node(i,b1,b2)->if i=n then true
								else (mem n b1)||(mem n b2)
