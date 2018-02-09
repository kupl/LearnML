type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with
	| ZERO -> (n2)
	| SUCC (nn1) -> SUCC(natadd nn1 n2);;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
  match n1 with 
	| ZERO -> ZERO
	| SUCC (nn1) -> (natadd (natmul nn1 n2) n2);;
