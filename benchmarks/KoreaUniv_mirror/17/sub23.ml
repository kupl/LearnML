(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
  Empty -> Empty
| Node (a,s1,s2) -> Node (a,mirror s2, mirror s1)
