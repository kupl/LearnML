type nat = ZERO | SUCC of nat

let rec natadd n1 n2 = match (n1, n2) with
	| (ZERO, _) -> n2
	| (_, ZERO) -> n1
	| (SUCC n1', _) -> SUCC (natadd n1' n2)

let rec natmul n1 n2 = match (n1, n2) with
	| (ZERO, _) | (_, ZERO) -> ZERO
	| (SUCC n1', _) -> natadd n2 (natmul n1' n2)

