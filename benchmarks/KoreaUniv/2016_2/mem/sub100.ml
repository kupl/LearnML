type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
  match tree with
  | Empty -> false
  | Node(num, left, right) -> if num=n then true
                              else if mem n left then true 
                              else  mem n right
