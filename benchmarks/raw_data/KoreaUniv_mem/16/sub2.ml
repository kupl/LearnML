type btree =
	| Empty
	| Node of int * btree * btree;;

let rec mem : int -> btree -> bool
= fun n tree -> match tree with
|Empty->false
|Node (a,b1tree,b2tree)-> if a=n then true
										 else if mem n b1tree then true
										 else if mem n b2tree then true else false;;
