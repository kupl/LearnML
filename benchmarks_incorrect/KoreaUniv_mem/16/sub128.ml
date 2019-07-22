type btree =
	| Empty
	| Node of int * btree * btree;;

(*let t1 = Node (1, Empty, Empty);;*)
(*let t2 = Node (1, Node(2,Empty,Empty), Node(3,Empty,Empty));;*)

let rec mem : int -> btree -> bool
= fun n tree -> 
  match tree with
  |Empty -> false 
  |Node(_,Empty,_) ->
    if n = 1 then true else false
  |Node(_,left,right) -> mem (n/2) left;;