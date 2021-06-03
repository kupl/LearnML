(* problem 1 *)
type btree = Empty | Node of int * btree * btree
let rec mirror : btree -> btree = fun t ->
    match t with
       Empty -> Empty
      |Node(k, left, right) -> Node(k, mirror right, mirror left);;

