
(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec f t = match t with 
           | Empty -> Empty
           | Node(n, l, r) -> Node(n, f r, f l) in
           f t
