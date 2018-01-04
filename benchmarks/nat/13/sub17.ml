(*2009-11718 1-3*)

type nat = ZERO | SUCC of nat

let rec natadd (n, m) = 
	match (n, m) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC b) -> (SUCC (natadd (ZERO, b)))
	| (SUCC a, ZERO) -> (SUCC (natadd (a, ZERO)))
	| (SUCC a, SUCC b) -> (SUCC (SUCC (natadd (a, b))))

let rec natmul (n, m) =
	match (n, m) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, SUCC b) -> ZERO
	| (SUCC a, ZERO) -> ZERO
	| (SUCC a, SUCC b) -> (match a with
						| ZERO -> natadd (ZERO, SUCC b)
						| _ -> natadd (SUCC b, natmul(a, SUCC b)))

