(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> match t with
  Node(x,l,r) -> Node(x,r,l)
  |Empty -> Empty;;
