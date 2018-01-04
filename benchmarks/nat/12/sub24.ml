type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) = match (n1, n2) with
	| (ZERO, n) | (n, ZERO) -> n
	| (_, SUCC n2') -> SUCC (natadd (n1, n2'))

let rec natmul (n1, n2) = match (n1, n2) with
	| (ZERO, n) | (n, ZERO) -> ZERO
	| (_, SUCC n2') -> natadd (n1, natmul (n1, n2'))