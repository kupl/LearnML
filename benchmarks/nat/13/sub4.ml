type nat 	= ZERO 
			| SUCC of nat

exception Exception_NegativeNumber

let rec result_of_nat n =
	match n with
	| ZERO -> 0
	| (SUCC i) -> 1 + (result_of_nat i)

let rec nat_of_int n =
	if n>0 then SUCC (nat_of_int (n-1))
    else if n=0 then ZERO
	else raise Exception_NegativeNumber

let natadd (e1, e2) = nat_of_int ((result_of_nat e1) + (result_of_nat e2))

let natmul (e1, e2) = nat_of_int ((result_of_nat e1) * (result_of_nat e2))