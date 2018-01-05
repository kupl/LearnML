type nat = 	ZERO | SUCC of nat

let rec calnat nat =
	match nat with
	| ZERO -> 0
	| (SUCC n) -> (calnat n) + 1

let natadd nat1 nat2 = (calnat nat1) + (calnat nat2)

let natmul nat1 nat2 = (calnat nat1) * (calnat nat2)
