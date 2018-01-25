type nat = ZERO | SUCC of nat;;

let rec natadd : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> (match n2 with
	| ZERO -> ZERO
	| _ -> let SUCC q = n2 in
		SUCC (natadd ZERO q))
| _ -> let SUCC p = n1 in
	SUCC (natadd p n2);;

let rec natmul : nat -> nat -> nat
=fun n1 n2 -> match n1 with
| ZERO -> ZERO
| SUCC ZERO ->
	(match n2 with
	| ZERO -> ZERO
	| _ -> let SUCC q = n2 in
		SUCC (natmul (SUCC ZERO) q))
| _ -> (match n2 with
	| ZERO -> ZERO
	| SUCC ZERO -> SUCC ZERO
	| _ -> let SUCC q = n2 in
		let SUCC p = n1 in
		SUCC (natmul p (natmul (SUCC ZERO) q)));;