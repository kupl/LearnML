type nat = ZERO | SUCC of nat

let natadd (n1, n2) =
	let rec eval_nat nat_ = 
	match nat_ with
		ZERO -> 0
		| SUCC nat -> (eval_nat nat) + 1
	in
	let rec build_nat n =
		if n = 0 then ZERO
			else SUCC (build_nat (n-1))
	in
	build_nat ((eval_nat n1) + (eval_nat n2))

let natmul (n1, n2) =
	let rec eval_nat nat_ =
	match nat_ with
		ZERO -> 0
		| SUCC nat -> (eval_nat nat) + 1
	in
	let rec build_nat n =
		if n = 0 then ZERO
			else SUCC (build_nat (n-1))
	in
	build_nat ((eval_nat n1) * (eval_nat n2))



