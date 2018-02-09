type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
  | Empty -> false
  | Node (x, left, right) -> n = x || (n < x && mem n left) || ( n > x && mem n right);;
