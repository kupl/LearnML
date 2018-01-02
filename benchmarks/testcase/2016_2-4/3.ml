type btree =
	| Empty
	| Node of int * btree * btree;;

let rec f : int -> btree -> bool
= fun n tree -> 
  match tree with
  |Empty -> false 
  |Node(_,Empty,_) ->
    if n = 1 then true else false
  |Node(_,left,right) -> f (n/2) left;;
