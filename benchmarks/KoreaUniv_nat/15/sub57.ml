type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun n1 n2 ->
	match n1 with
	|ZERO -> n2
	|SUCC x -> SUCC (natadd x n2)
;;

let rec natmul : nat -> nat -> nat
=fun n1 n2 ->
	if n2 = ZERO then 
		ZERO
	else
		match n1 with
		|ZERO -> ZERO
		|SUCC x -> if x = ZERO then 
					  n2
				   else
					  natadd (natmul x n2) n2
;;
	
	













