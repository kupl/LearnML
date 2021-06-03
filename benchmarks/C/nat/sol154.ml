type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> ZERO 
let rec natadd n1 n2 = 
	match n2 with
	|ZERO -> 
		let rec e n = 
			match n with
			|ZERO -> ZERO
			|SUCC (nk) -> SUCC (e nk)
		in
		e n1
	|SUCC (k) -> SUCC (natadd n1 k)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> ZERO 
let rec natmul n1 n2 =
	match n2 with
	|ZERO -> ZERO
	|SUCC (k) -> natadd n1 (natmul n1 k)
