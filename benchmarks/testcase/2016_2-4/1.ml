type btree =
	| Empty
	| Node of int * btree * btree

let rec f x n = match n with
		| Empty -> false
		| Node (k, left, right) -> if x<=0 then false else
	        if k<x then f x right
		else if k=x then true 
		else if k>x then true
		else false;;
