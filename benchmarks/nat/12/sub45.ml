type nat = ZERO 
	| SUCC of nat

let rec natadd (n1, n2) =
	match (n1, n2) with
	| (ZERO, ZERO) -> ZERO
	| (SUCC nn1, SUCC nn2) -> SUCC (SUCC (natadd (nn1, nn2)))
	| (SUCC n, _) -> SUCC (natadd (n, n2))
	| (_, SUCC n) -> SUCC (natadd (n1, n))

let rec natmul (n1, n2) =
	match (n1, n2) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (_, SUCC n) -> (natadd (n1, (natmul (n1, n))))
