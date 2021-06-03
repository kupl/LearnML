type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree -> 
match tree with
Empty -> false
|Node (x, left, right) ->
if n = x then true else mem n left || mem n right;;
