type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
			match n1 with
			|ZERO -> n2
			|SUCC(k) -> SUCC(natadd k n2)
let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
			match n1 with
			|ZERO -> ZERO
			|SUCC(k) -> natadd n2 (natmul k n2)
