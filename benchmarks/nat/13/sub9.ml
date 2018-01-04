type nat =
	ZERO
	| SUCC of nat
let rec nattonum nat =
	match nat with
	| ZERO -> 0
	| SUCC n -> 1 + (nattonum n)
let rec numtonat num =
	if num = 0 then ZERO
	else SUCC (numtonat (num - 1))
let natadd (a, b) =
	numtonat ((nattonum a) + (nattonum b))
let natmul (a, b) =
	numtonat ((nattonum a) * (nattonum b))
