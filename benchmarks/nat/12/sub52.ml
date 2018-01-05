type nat = ZERO | SUCC of nat

let rec natadd = (fun (a, b) -> match (a, b) with
				| (ZERO, ZERO) -> ZERO
				| (ZERO, SUCC b) -> (SUCC (natadd (ZERO, b)))
				| (SUCC a, ZERO) -> (SUCC (natadd (a, ZERO)))
				| (SUCC a, SUCC b) -> (SUCC (SUCC (natadd (a, b))))
	);;

let rec natmul = (fun (a, b) -> (match a with
					| (SUCC ZERO) -> b
					| (SUCC a) -> (natadd (b, (natmul (a, b))))
					| ZERO -> ZERO
				)
	);;