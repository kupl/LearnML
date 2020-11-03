(*problem 1*)
type btree = Empty |Node of int * btree * btree

let rec mirror : btree -> btree = fun t ->
match t with
|Empty -> Empty
|Node(l,v,r) -> Node(l,mirror r, mirror v);;
