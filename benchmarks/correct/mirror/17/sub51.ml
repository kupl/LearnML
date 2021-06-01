type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
=fun t -> 
  match t with 
  | Empty -> Empty
  | Node (a, left, right) -> Node (a, mirror right, mirror left) ;;
