type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	match n1 with
	|	ZERO -> n2
	|	SUCC t -> natadd t (SUCC n2)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let rec loop a r =
		match a with
		|	ZERO -> r
		|	SUCC t -> loop t (natadd n2 r) in
	loop n1 ZERO
