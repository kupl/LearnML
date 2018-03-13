(*problem1*)

type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun tree ->
	match tree with 

	| Node(i,t1, t2) -> Node (i, mirror t2, mirror t1)
	| Empty -> Empty
