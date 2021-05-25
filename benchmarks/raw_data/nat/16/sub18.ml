type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
	| ZERO -> (match n2 with
		| ZERO -> ZERO
		| SUCC (succ_n2) -> SUCC (natadd n1 succ_n2)
		)
	| SUCC (succ_n1) -> SUCC (natadd succ_n1 n2);;(* TODO *)

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> match n1 with 
	| ZERO -> ZERO
	| SUCC (succ_n1) ->
		(match n2 with
		| ZERO -> ZERO
		| SUCC (succ_n2) -> natadd n1 (natmul n1 succ_n2)
		);; (* TODO *)
