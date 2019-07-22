(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec swap f = match f with | Empty -> Empty | Node(a,x,y) -> Node(a, (swap y) ,(swap x)) in swap t;;
