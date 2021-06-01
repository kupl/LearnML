(* problem 1*)
type btree = Empty | Node of int * btree * btree

let combine n l r = Node (n, l, r)

let rec mirror : btree -> btree
= fun t -> match t with | Empty -> t | Node (n, Empty, Empty) -> t | Node (n, l, r) -> combine (n) (mirror r) (mirror l)

