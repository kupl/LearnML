type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->  (* TODO *)

	match n1 with
	|ZERO -> n2
	|SUCC( subn1 ) -> SUCC( natadd subn1 n2 ) (*subn1은 n1에서 1을뺀값*)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
	if(n1=ZERO || n2=ZERO) then ZERO
	else
	let rec innerLoop : nat -> nat -> nat -> nat
	= fun n1 n2 maintain ->
			
			match n1 with
			|SUCC ZERO -> n2
			|SUCC( sub_count ) -> innerLoop sub_count (natadd maintain n2) maintain	

	in innerLoop n1 n2 n2
