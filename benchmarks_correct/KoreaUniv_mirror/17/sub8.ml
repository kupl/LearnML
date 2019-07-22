(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t -> match t with
| Empty -> Empty
| Node (p, l, r) -> Node (p, mirror r, mirror l)
