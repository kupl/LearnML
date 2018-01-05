 type nat = ZERO | SUCC of nat

 let rec natadd (a, b) =
 	match (a, b) with
	| (ZERO, b) -> b
	| (a, ZERO) -> a
	| (a, SUCC b) -> natadd (SUCC a, b)

let rec natmul (a, b) =
	match (a, b) with
	| (ZERO, b) -> ZERO
	| (a, ZERO) -> ZERO
	| (a, SUCC b) -> natadd (natmul (a, b), a)
