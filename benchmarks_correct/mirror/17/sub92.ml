(*problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree = fun t ->
match t  with
|Empty -> Empty
|Node(n, btree1, btree2) -> Node(n, mirror btree2, mirror btree1);;
