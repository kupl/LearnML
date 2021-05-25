type nat = ZERO | SUCC of nat

let rec natadd n1 n2 = match n2 with
		| ZERO -> let rec nat n3 = match n3 with
					| ZERO -> ZERO
					| SUCC (n4) -> SUCC(nat n4) in nat n1
		| SUCC (n) -> SUCC (natadd n1 n)

let rec natmul n1 n2 = match n2 with
		| ZERO -> ZERO
		| SUCC (c) -> natadd n1 (natmul n1 c)

