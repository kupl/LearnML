type nat = ZERO | SUCC of nat
let rec natadd (n1, n2) =
	match n1 with
		| ZERO -> n2
		| SUCC n1_ -> natadd (n1_, SUCC n2)
let rec natmul (n1, n2) =
	match n1 with
		| ZERO -> ZERO
		| SUCC ZERO -> n2
		| SUCC n1_ -> natadd (natmul (n1_, n2), n2)
		
(* TEST SET *)
(*
natadd (ZERO, ZERO);;
natadd (ZERO, (SUCC (SUCC ZERO)));;
natadd ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO))));;
natmul (ZERO, (SUCC (SUCC ZERO)));;
natmul ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO))));;
*)