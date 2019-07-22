(*problem 1*)

type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
match t with
|Empty -> Empty
|Node(n, a, b) -> Node(n, mirror b, mirror a);;
