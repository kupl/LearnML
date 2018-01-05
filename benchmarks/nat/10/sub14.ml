type nat = ZERO | SUCC of nat

let rec natadd (nat1, nat2) =
	match nat1 with
		ZERO -> nat2
		| SUCC nat3 -> SUCC (natadd (nat3, nat2));;

let rec natmul (nat1, nat2) =
	match nat1 with
		ZERO -> ZERO
		| SUCC nat3 ->(
			match nat2 with
				ZERO -> ZERO
				| SUCC nat4 -> natadd (nat2, (natmul (nat3, nat2)))
			);;

(*
let testnat1 = SUCC (SUCC (SUCC (SUCC (SUCC ZERO))));;
let testnat2 = SUCC (SUCC ZERO);;
*)
