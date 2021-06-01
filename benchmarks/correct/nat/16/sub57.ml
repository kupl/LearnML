type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	let rec add a = function
		| ZERO -> a
		| SUCC(nat) -> add (SUCC a) nat in
	if n1 = ZERO then add n2 n1 else add n1 n2

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let rec mul n1 n2 = function
			| ZERO -> ZERO
			| SUCC(ZERO) -> n1
			| SUCC(nat) -> mul (natadd n1 n2) n2 nat in
	if n1 = ZERO then mul n2 n2 n1 else mul n1 n1 n2
