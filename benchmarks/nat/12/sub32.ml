type nat = ZERO | SUCC of nat

let rec subAdder(nat1, nat2) =
	match nat1 with
	| ZERO -> nat2
	| SUCC snat1 -> subAdder(snat1, (SUCC nat2))

let natadd (nat1, nat2) =
	match nat1, nat2 with
	| ZERO, _ -> nat2
	| _, ZERO -> nat1
	| _, _	-> subAdder(nat1, nat2)

let rec natmul (nat1, nat2) =
	match nat1, nat2 with
	| ZERO, _ 
	| _, ZERO -> ZERO
	| SUCC ZERO, snat2 -> snat2
	| SUCC snat1, snat2 -> natadd(snat2,  natmul(snat1,snat2))

	
