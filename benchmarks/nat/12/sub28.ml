type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
	match nat1 with
	ZERO ->	nat2
	|SUCC a ->
		match nat2 with
		ZERO ->	nat1
		|_ ->	SUCC (natadd (a, nat2))

let rec natmul (nat1, nat2) =
	match nat1 with
	ZERO ->	ZERO
	|SUCC a ->
		match nat2 with
		ZERO ->	ZERO
		|_ ->	natadd (natmul (a, nat2), nat2)
