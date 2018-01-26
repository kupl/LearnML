(* problem 1 *)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
            | Empty -> t
            | Node(int,x,y) -> Node(int,(mirror y),(mirror x))
