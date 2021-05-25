type btree =
	| Empty
	| Node of int * btree * btree

let rec treeGetnumber: btree -> int
= fun tree -> match tree with
	|Empty -> raise (Failure "is empty")
	|Node(n, child1, child2) -> n

let rec treeGetchild1: btree -> btree
= fun tree -> match tree with
	|Empty -> raise (Failure "is empty")
        |Node(n, child1, child2) -> child1

let rec treeGetchild2: btree -> btree
= fun tree -> match tree with
	|Empty -> raise (Failure "is empty")
        |Node(n, child1, child2) -> child2

let rec mem : int -> btree -> bool
= fun n tree -> 
if tree = Empty then false 
else if  n = (treeGetnumber tree) then true 
else (mem n (treeGetchild1 tree)) || (mem n (treeGetchild2 tree))
