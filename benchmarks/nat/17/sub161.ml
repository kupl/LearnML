type nat = ZERO | SUCC of nat

let rec natadd ((a, b) : nat * nat) = match a with
	| ZERO -> b
	| SUCC (a_succ) -> SUCC (natadd (a_succ, b))

let rec natmul ((a, b): nat * nat) = match a with
	| ZERO -> ZERO
	| SUCC (a_succ) -> natadd (natmul (a_succ, b), b)
