type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
	match n1, n2 with
	| ZERO, ZERO -> ZERO
	| ZERO, SUCC(natval) -> n2
	| SUCC(natval), ZERO -> n1
	| SUCC(natn1), SUCC(natn2) -> SUCC( SUCC( natadd natn1 natn2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
	match n1, n2 with
	| ZERO, ZERO -> ZERO
	| ZERO, SUCC(natval) -> ZERO
	| SUCC(natval), ZERO -> ZERO
	| SUCC(natn1), SUCC(natn2) -> natadd (natmul natn1 n2) n2

