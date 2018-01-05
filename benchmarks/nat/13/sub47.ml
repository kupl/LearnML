type nat = ZERO | SUCC of nat

let rec natadd nat_pair =
	match nat_pair with
	| (a, ZERO) -> a
	| (a, SUCC pre_b) -> SUCC (natadd (a, pre_b))

let rec natmul nat_pair =
	match nat_pair with
	| (a, ZERO) -> ZERO
	| (a, SUCC pre_b) -> natadd (a, natmul (a, pre_b))
