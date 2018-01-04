type nat = ZERO | SUCC of nat

let rec natadd (a,b) =
	match (a,b) with
	| (ZERO,b) -> b
	| (a, ZERO) -> a
	| (SUCC a ,b) ->  SUCC (natadd (a, b))

let rec natmul (a,b) =
	match (a,b) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC ZERO, b) -> b
	| (a, SUCC ZERO) -> a 
	| (SUCC a, b) -> natadd (b, natmul (a,b))
	