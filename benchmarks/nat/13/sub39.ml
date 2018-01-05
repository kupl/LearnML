type nat = ZERO | SUCC of nat

let rec makenat i =
	if i = 0 then ZERO
	else (SUCC (makenat (i - 1)))

let rec calnat n =
	match n with
	| SUCC p -> 1 + (calnat p)
	| ZERO -> 0

let natadd (nat1, nat2) = makenat ((calnat nat1) + (calnat nat2))
let natmul (nat1, nat2) = makenat ((calnat nat1) * (calnat nat2))
