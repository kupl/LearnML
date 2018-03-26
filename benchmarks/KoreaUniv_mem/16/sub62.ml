type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
    Node (m, a, b) -> n = m || mem n a || mem n b
  | Empty          -> false ;; 
