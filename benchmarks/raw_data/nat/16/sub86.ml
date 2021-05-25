type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with 
		|ZERO -> n1
		|SUCC tl -> SUCC (natadd n1 tl)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
		|ZERO -> ZERO
		|SUCC ZERO -> n2
		|SUCC tl -> natadd n2 (natmul tl n2) 
