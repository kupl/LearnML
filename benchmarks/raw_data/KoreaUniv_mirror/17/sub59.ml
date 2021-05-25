(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
    match t with
    |Empty -> Empty
    |Node(x,Empty,Empty) -> Node(x,Empty,Empty)
    |Node(x,lt,rt) -> Node(x,mirror rt,mirror lt);;