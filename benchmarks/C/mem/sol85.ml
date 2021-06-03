type btree =
	| Empty
	| Node of int * btree * btree

let getLeftChild: btree -> btree
= fun tree -> match tree with 
|Empty -> raise(Failure "Tree does not have a left child.")
|Node (a,b,c) -> b

let getRightChild: btree -> btree
= fun tree -> match tree with 
|Empty -> raise(Failure "Tree does not have a right child.")
|Node (a,b,c) -> c

let getInt: btree -> int
= fun tree -> match tree with 
|Empty -> raise(Failure "Tree is empty.")
|Node (a,b,c) -> a

let rec mem : int -> btree -> bool
= fun n tree -> 
if tree = Empty then false 
else if n = getInt tree then true
else mem n (getLeftChild tree) || mem n (getRightChild tree)
