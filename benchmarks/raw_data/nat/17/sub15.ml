(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
	 	          | ZERO -> n2
		          | SUCC nat -> if nat = ZERO then SUCC n2
				                    else SUCC (natadd nat n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> match n1 with
		          | ZERO -> ZERO
		          | SUCC nat -> if nat = ZERO then n2
			      	            else natadd (natmul (SUCC ZERO) n2) (natmul nat n2)

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> match n2 with
		          | ZERO -> SUCC ZERO
		          | SUCC nat -> if nat = ZERO then n1
				                    else natmul (natexp n1 (SUCC ZERO)) (natexp n1 nat)
