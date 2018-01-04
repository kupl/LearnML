type nat = ZERO | SUCC of nat;;

let rec natToInt natural =
	match natural with
	|ZERO -> 0
	|SUCC (natural2) -> 1 + natToInt (natural2)
	;;

let rec intToNat integer = 
	if integer = 0 then ZERO
	else SUCC (intToNat (integer-1))
	;;

let natadd (a, b) =
	intToNat( (natToInt a + natToInt b))

let natmul (a, b) =
	intToNat( (natToInt a * natToInt b))
