type nat = ZERO | SUCC of nat

let rec eval x =
	match x with
	| ZERO -> 0
	| SUCC p -> eval (p) + 1

let rec rebuild x = 
	if x = 0 then ZERO
	else SUCC(rebuild (x-1))

let natadd (x, y) = rebuild(eval x + eval y)

let natmul (x, y) = rebuild(eval x * eval y)
