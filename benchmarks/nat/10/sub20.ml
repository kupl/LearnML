type nat = ZERO | SUCC of int 

let natadd n1 n2 =
	match (n1,n2) with
		(ZERO,ZERO) -> ZERO
		| (ZERO,SUCC m) -> SUCC m
		| (SUCC m,ZERO) -> SUCC m
		| (SUCC m1,SUCC m2) -> SUCC (m1 + m2);;
let natmul n1 n2 =
	match (n1,n2) with
		(ZERO,ZERO) -> ZERO
		| (ZERO,SUCC m) -> ZERO
		| (SUCC m,ZERO) -> ZERO
		| (SUCC m1,SUCC m2) -> SUCC (m1 * m2)
;;

