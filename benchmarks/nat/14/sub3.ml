type nat = ZERO | SUCC of nat

let rec nat_to_int nat = 
	match nat with
	|ZERO -> 0
	|SUCC a -> (nat_to_int a) + 1

let rec int_to_nat a = 
	if a = 0 then ZERO
	else SUCC (int_to_nat (a - 1))

let natadd (a, b) = int_to_nat ((nat_to_int a) + (nat_to_int b))
let natmul (a, b) = int_to_nat ((nat_to_int a) * (nat_to_int b))
