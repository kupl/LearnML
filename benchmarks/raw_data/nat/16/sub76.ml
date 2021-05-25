type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	match n1, n2 with
	ZERO,ZERO -> ZERO
|	SUCC nat1,ZERO -> n1
|	ZERO,SUCC nat2 -> n2
|	SUCC nat1 ,SUCC nat2  -> natadd (SUCC n1) nat2

let rec natmul : nat -> nat -> nat
= fun n1 n2 ->
	match n1, n2 with
	ZERO,_ -> ZERO
|	_,ZERO -> ZERO
|	SUCC nat1 ,SUCC nat2 ->
		if nat2 = ZERO then
			n1
		else
			natadd (natmul n1 nat2) n1
