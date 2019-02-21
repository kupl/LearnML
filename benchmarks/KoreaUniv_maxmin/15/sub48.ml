let rec max : int list -> int
=fun l ->
	match l with
 	[] -> 0
	| head :: [] -> head
	| head :: tail ->
		if head > max tail then head
		else max tail

let rec min : int list -> int
=fun l -> 
	match l with
 	[] -> 0
	| head :: [] -> head
	| head :: tail -> 
		if head < min tail then head
		else min tail

