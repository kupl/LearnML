type nat = ZERO | SUCC of nat

let rec natadd (a, b) = 
	match (a, b) with 
	|(ZERO, ZERO) -> ZERO
	|(ZERO, _) -> b
	|(_, ZERO) -> a
	|(SUCC n, SUCC m) -> SUCC(SUCC(natadd (n, m)))

let rec natmul (a, b) = 
	match a with
	|ZERO -> ZERO
	|SUCC n -> (natadd(natmul(n, b), b))
