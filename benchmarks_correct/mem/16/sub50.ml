type btree =
	| Empty
	| Node of int * btree * btree

let rec mem a bt = 
	match bt with
	| Empty -> false
	| Node (i,b1,b2) ->
		if i = a then true
		else if mem a b1 then true
		else mem a b2   
