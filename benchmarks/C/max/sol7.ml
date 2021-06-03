let rec max : int list -> int
=fun l ->
	match l with
	| [] -> raise (Failure "Emplty list!")
	| x::[] -> x
	| x::y -> 
		let temp = max y in
		if x > temp then x else temp
