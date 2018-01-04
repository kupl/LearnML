type nat = ZERO | SUCC of nat

let rec natadd (n1,n2) = match (n1,n2) with
	| (ZERO, n) | (n, ZERO) -> n
	| (SUCC n, _) -> natadd (n,(SUCC n2))

let rec natmul (n1,n2) = match (n1,n2) with
	| (ZERO, _) | (_, ZERO) -> ZERO
	| (SUCC n1', _) -> natadd (n2,(natmul (n1',n2)))

