let rec max : int list -> int
=fun l ->
	match l with
	| [] -> 0
	| x :: [] -> x
	| x :: xm -> 
		let v = max xm in
		if x<v then v
		else x

let rec min : int list -> int
=fun l ->
	match l with
	| [] -> 0
	| x :: [] -> x
	| x :: xs ->
		let v = min xs in
		if x<v then x
		else v
