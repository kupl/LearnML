type btree =
	| Empty
	| Node of int * btree * btree

let rec f : int -> btree -> bool
= fun n tree ->
  match tree with
  | Empty -> false
  | Node(num, left, right) -> if num=n then true
                              else if f n left then true 
                              else  f n right
