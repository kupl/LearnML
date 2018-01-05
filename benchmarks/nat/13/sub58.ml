type nat = ZERO | SUCC of nat

let rec natadd n = match n with
	| (ZERO, n2) -> n2
	| (n1, ZERO) -> n1
	| (SUCC n1, n2) -> natadd (n1, SUCC n2)

let rec natmul n = match n with
	| (ZERO, n2) -> ZERO
	| (n1, ZERO) -> ZERO
	| (SUCC ZERO, n2) -> n2
	| (SUCC n1, n2) -> natadd (n2, natmul (n1, n2))
