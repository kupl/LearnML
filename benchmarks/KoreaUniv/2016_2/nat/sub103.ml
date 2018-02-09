type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
	match n1, n2 with
	|	SUCC(nat1), SUCC(nat2) -> SUCC(SUCC(natadd nat1 nat2))
	| ZERO, SUCC(nat2) -> SUCC(natadd ZERO nat2)
	| SUCC(nat1), ZERO -> SUCC(natadd nat1 ZERO)
	| ZERO, ZERO -> ZERO

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n1, n2 with
	| ZERO, _ -> ZERO
	| _, ZERO -> ZERO
	| SUCC(nat1), SUCC(nat2) -> natadd n1 (natmul n1 nat2) 
