type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> ZERO
let rec natadd q w =
	match q with
	|ZERO -> w
	|SUCC(a) -> natadd a (SUCC(w))

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> ZERO
let rec natmul q w =
	match q with
	|SUCC ZERO -> w
	|SUCC(e) -> natadd (natmul e w) w