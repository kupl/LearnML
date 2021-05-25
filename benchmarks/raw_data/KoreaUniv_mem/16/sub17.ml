type btree =
	| Empty
	| Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with
| Empty -> false
| Node (parent, left, right) ->
    parent = n || mem n left || mem n right;;
