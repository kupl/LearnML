type nat = ZERO | SUCC of nat

let rec natadd n =
	match n with
		(ZERO, ZERO) -> ZERO
		|(ZERO, m) -> m
 		|(m, ZERO) -> m
		|(m1, SUCC m2) -> natadd (SUCC m1, m2)

let rec natmul n =
	match n with
		(ZERO, ZERO) -> ZERO
		|(ZERO, _) -> ZERO
		|(_, ZERO) -> ZERO
		|(m, SUCC m2) -> natadd (m, natmul (m, m2))