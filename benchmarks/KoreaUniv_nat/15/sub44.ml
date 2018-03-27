type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> 
let rec length n =
	match n with
  	ZERO -> 0
	| SUCC(a) -> 1 + length a
in let rec result m =
	if m=0 then ZERO
	else SUCC(result (m-1))
in result ((length n1)+(length n2))

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> 
let rec length n =
	match n with
  	ZERO -> 0
	| SUCC(a) -> 1 + length a
in let rec result m =
	if m=0 then ZERO
	else SUCC(result (m-1))
in result ((length n1)*(length n2))

