type nat = ZERO | SUCC of nat

let rec nat_to_num n =
	match n with 
	| ZERO -> 0
	| SUCC n1 -> 1 + nat_to_num n1

let rec num_to_nat n =
	if n=0 then ZERO
	else SUCC (num_to_nat(n-1))

let natadd(n1, n2) = num_to_nat(nat_to_num n1 + nat_to_num n2)
let natmul(n1, n2) = num_to_nat(nat_to_num n1 * nat_to_num n2)
