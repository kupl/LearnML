type nat = ZERO | SUCC of nat
let rec nat2num nat = 
	match nat with
	| ZERO -> 0
	| SUCC n -> succ(nat2num n)
let rec natadd(nat1,nat2) = 
	match nat1 with
	| ZERO -> nat2
	| SUCC n -> SUCC(natadd(n,nat2))
let rec natmul(nat1,nat2) = 
	let rec natmul_r nat1 temp nat2 =
		match nat2 with
		| ZERO -> temp
		| SUCC n -> natmul_r nat1 (natadd(nat1,temp)) n
	in
	match nat1 with
	|ZERO -> (match nat2 with
					|ZERO -> ZERO
					|SUCC _ -> ZERO)
	|SUCC _ -> (match nat2 with
					|ZERO -> ZERO
					|SUCC r -> natmul_r nat1 ZERO nat2)
