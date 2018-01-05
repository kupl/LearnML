type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
		match a with
		| ZERO -> b
		| SUCC (nat2) ->
			SUCC (natadd(nat2, b));;
			

let rec natmul (a, b) =
	match (a,b) with
		| (_,ZERO) -> ZERO
		| (ZERO,_)  -> ZERO
		| (SUCC nat1, SUCC nat2) ->
			natadd(natmul(nat1, b),b);;