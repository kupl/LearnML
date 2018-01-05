(* 2009-13384, CHO Hyunik *)




type nat = ZERO | SUCC of nat

let rec natadd(a, b) = 
	match (a, b) with
	(ZERO, _) -> b
	| (_, ZERO) -> a
	| (SUCC(c), _) -> SUCC(natadd(c, b))

let rec natmul(a, b) =
	match (a, b) with
	(ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC(c), _) -> natadd(a, natmul(c, b))