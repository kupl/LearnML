(* problem 1 *)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
            | Empty -> t
            | Node(n,x,y) -> Node(n,(mirror y),(mirror x))
