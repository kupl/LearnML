type nat = ZERO | SUCC of nat

let rec natadd ((n,m) : nat * nat) : nat =
	match n, m with
		ZERO, _ -> m
	| _, ZERO -> n
	| SUCC k, _ -> natadd (k, SUCC m)

let rec natmul ((n,m) : nat * nat) : nat =
	match n, m with
		ZERO, _
	| _, ZERO -> ZERO
	| SUCC k, _ -> natadd (natmul (k, m), m)
