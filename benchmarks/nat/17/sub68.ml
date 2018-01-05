type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat = 
	fun (a,b) -> match a with
		| ZERO -> b
		| SUCC x -> natadd (x, SUCC b)

let rec natmul : nat * nat -> nat = 
	fun (a,b) -> match a with
		| ZERO -> ZERO
		| SUCC x -> natadd(b, natmul(x,b))

