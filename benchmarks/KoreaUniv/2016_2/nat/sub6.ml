type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> let rec evalnat n = 
					    match n with
							| ZERO -> ZERO
							| SUCC (n3) -> SUCC (evalnat n3)
in evalnat n1
| SUCC (b2) -> SUCC (natadd n1 b2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> ZERO
| SUCC (b2) -> natadd n1 (natmul n1 b2)
