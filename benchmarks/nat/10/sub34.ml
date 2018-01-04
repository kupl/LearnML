type nat = ZERO | SUCC of nat

let rec nat_to_int nat =
	match nat with
	  ZERO -> 0
	| SUCC (n) -> 1 + (nat_to_int n)

let natadd n1 n2 =
	(nat_to_int n1) + (nat_to_int n2)
let natmul n1 n2 =
	(nat_to_int n1) * (nat_to_int n2)

