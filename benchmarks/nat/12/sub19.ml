(* 2008-11874 EXERCISE 7 *)

type nat = ZERO | SUCC of nat

let rec natadd(n1,n2) =
	match(n1,n2) with
		| (ZERO,_) -> n2
		| (SUCC(z1),_) -> SUCC(natadd(z1,n2))

let rec natmul(n1,n2) =
	match(n1,n2) with
		| (ZERO,_) -> ZERO
		| (SUCC(z1),_) -> natadd(n2,(natmul(z1,n2)))