type nat = ZERO
		| SUCC of nat

let rec natadd (n1, n2) =
	match (n1, n2) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC(n)) -> SUCC(n)
	| (SUCC(n), ZERO) -> SUCC(n)
	| (SUCC(_n1), SUCC(_n2)) -> natadd (_n1, (SUCC(SUCC(_n2))))

let rec natmul (n1, n2) =
	match (n1, n2) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC(n)) -> ZERO
	| (SUCC(n), ZERO) -> ZERO
	| (SUCC(_n1), SUCC(_n2)) -> natadd(SUCC(_n2), natmul(_n1, SUCC(_n2))) 
