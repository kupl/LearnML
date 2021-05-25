(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror 
= fun t ->
match t with
| Empty -> Empty
| Node(n,Empty,Empty) -> Node(n, Empty, Empty)
| Node(n,Empty,r) -> Node(n,(mirror r), Empty)
| Node(n,l,Empty) -> Node(n,Empty,(mirror l))
| Node(n,l,r) -> Node(n,(mirror r),(mirror l));;
