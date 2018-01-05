type nat = ZERO | SUCC of nat

let rec natcal n = 
	match n with
		ZERO -> 0
		| SUCC(r) -> 1 + (natcal r)

let rec tonat i = 
	match i with
		0 -> ZERO
		| _ -> (SUCC(tonat (i-1)))

let natadd (nat1, nat2)= (tonat ((natcal nat1) + (natcal nat2))) 


let natmul (nat1, nat2)= (tonat ((natcal nat1) * (natcal nat2)))		
