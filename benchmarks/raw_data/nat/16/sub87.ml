type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
		| ZERO -> n1
		| SUCC (a) -> natadd (SUCC (n1)) a

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	if n1=ZERO || n2=ZERO then
		ZERO
	else
		match n2 with
			| SUCC (a) -> natadd (natmul n1 a) n1
			| ZERO -> ZERO
