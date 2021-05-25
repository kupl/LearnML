type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
  | Empty -> false
  | Node (x, Empty, Empty) -> if x == n then true
                              else false
  | Node (x, b1, b2) -> if x != n then mem n b1 || mem n b2
                        else true
