(* problem 1 *)
type btree = Empty | Node of int * btree * btree

let mirror : btree->btree
=fun t -> let rec f t = 
match t with 
 | Empty -> Empty
 | Node(n,tree1,tree2) -> Node(n, f tree2, f tree1)
in f t
