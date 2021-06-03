(*problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t ->
let rec mirro t =
match t with
| Empty -> Empty
| Node (x, y, z) -> Node (x, mirro z, mirro y) in mirro t;;