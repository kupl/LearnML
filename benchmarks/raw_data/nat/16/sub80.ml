type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  match n2 with |ZERO ->n1 |SUCC c -> SUCC(natadd n1 c);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n2 with
	|ZERO -> ZERO
	|SUCC(c) -> natadd n1 (natmul n1 c);;
