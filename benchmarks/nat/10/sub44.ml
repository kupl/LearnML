type nat = ZERO | SUCC of nat

let rec natadd (a, b) = match a
	with ZERO -> b
	| SUCC ap -> SUCC (natadd (ap, b))

let rec natmul (a, b) =
	if a=ZERO || b=ZERO then ZERO
	else match a
	with SUCC ZERO -> b
	| SUCC ap -> (natadd (a, (natmul (ap, b))));;
