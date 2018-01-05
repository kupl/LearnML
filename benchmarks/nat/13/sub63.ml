type nat = ZERO | SUCC of nat

let rec findint a = 
	match a with
	| ZERO -> 0
	| SUCC(n) -> 1+(findint n)

let rec nataddint (a, b) =
	(findint a) + (findint b)

let natmulint (a, b) = (findint a)*(findint b)

let rec makenat i = 
	if i = 0 then ZERO
	else SUCC(makenat (i-1))

let natadd (a, b) = makenat (nataddint(a, b))

let natmul (a, b) = makenat (natmulint(a, b))
