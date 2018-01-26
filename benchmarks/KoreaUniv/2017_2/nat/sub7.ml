(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		| SUCC(x) -> SUCC(natadd x n2)
		| ZERO -> match n2 with
				| SUCC(y) -> SUCC(natadd ZERO y)
				| ZERO -> ZERO

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
		| SUCC(x) -> (natadd (natmul n1 x) n1)
		| ZERO -> ZERO

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
		| SUCC(x) -> (natmul (natexp n1 x) n1)
		| ZERO -> SUCC(ZERO)
