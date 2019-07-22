(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  |Empty -> Empty
  |Node(num,b1,b2) ->
    Node(num,mirror(b2),mirror(b1))
    