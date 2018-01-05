type nat = ZERO
		| SUCC of nat

let rec natadd (n1, n2) =
	match (n1, n2) with
	| (ZERO, n2) -> n2
	| (n1, ZERO) -> n1
	| (SUCC na, SUCC nb) -> SUCC(SUCC(natadd (na,nb)))

let rec natmul (n1, n2) =
	match (n1, n2) with
	| (ZERO, n2) -> ZERO
	| (n1 , ZERO) -> ZERO
	| (SUCC(ZERO), n2) -> n2
	| (n1, SUCC(ZERO)) -> n1
	| (SUCC na, SUCC nb) -> natadd(natadd(SUCC(ZERO),natadd(na,nb)),natmul(na,nb))
