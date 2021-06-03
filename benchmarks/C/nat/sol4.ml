type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
	match n1, n2 with
	| SUCC x, SUCC y -> SUCC( SUCC( natadd x y )) 
	| SUCC x, ZERO -> SUCC( natadd x ZERO )
	| ZERO, SUCC y -> SUCC( natadd ZERO y )
	| ZERO, ZERO -> ZERO

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
	match n1, n2 with
	| SUCC x, SUCC y -> natadd (natmul x n2) n2
	| SUCC x, ZERO -> ZERO
	| ZERO, SUCC y -> ZERO
	| ZERO, ZERO -> ZERO
