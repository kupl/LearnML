type nat = | ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
	= fun n1 n2 ->
		match n1 with
			| ZERO -> n2
			| SUCC n -> natadd n (SUCC n2);;

let rec natmul : nat -> nat -> nat
	= fun n1 n2 ->
		match n1 with
			| ZERO -> (*if n2=ZERO then ZERO
								else n2*)ZERO
			| SUCC ZERO -> n2
			| SUCC n -> natadd n2 (natmul n n2);;
