type btree =
	| Empty
	| Node of int * btree * btree
let rec mem n t =
match t with
|Empty -> false
|Node (k,t1,t2) -> (k==n)||(mem n t1)||(mem n t2)
