(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec mr t = match t 
with Empty -> Empty
| Node(a, b, c)-> Node(a, mr c, mr b)
in mr t
