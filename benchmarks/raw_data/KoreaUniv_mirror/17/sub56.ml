(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec mir t = match t with | Empty -> Empty | Node(n, a, b) -> Node(n,mir b,mir a) in mir t
