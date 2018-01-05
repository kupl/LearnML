type nat = ZERO | SUCC of nat

let rec nat_num nat1 =
	match nat1 with
	| ZERO -> 0
	| SUCC nat2 -> (nat_num nat2) + 1

let rec num_nat num1 =
	if num1 = 0 then ZERO
	else SUCC (num_nat (num1 - 1))

let natadd (n1 , n2) = num_nat ((nat_num n1) + (nat_num n2))
	
let natmul (n1 , n2) = num_nat ((nat_num n1) * (nat_num n2))



