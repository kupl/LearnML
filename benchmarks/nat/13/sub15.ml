type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match b with
	|ZERO -> a
	|SUCC x -> natadd((SUCC a), x)

let rec my_natmul t (a, b) =
	match b with
	|ZERO -> ZERO
	|SUCC x -> 
		if x=ZERO then a
		else my_natmul t (natadd (a, t), x)

let natmul (a, b) = my_natmul a (a, b)
