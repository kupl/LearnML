type nat = ZERO | SUCC of nat


	
let rec natadd (n, m) =
	match n with 
	| ZERO -> m
	| SUCC x' -> SUCC (natadd (x', m))

let rec natmul (n, m) =
	match n with
	| ZERO -> ZERO
	| SUCC ZERO -> m
	| SUCC x -> 
		(match m with
		| ZERO -> ZERO
		| SUCC ZERO -> n
		| _ -> natadd (m, natmul(x, m)))