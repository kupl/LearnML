(* HW1-Exercise 4*)
type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) = 
	match (n1, n2) with
	| (ZERO, _) -> n2
	| (SUCC(n3), _) -> natadd (n3, SUCC(n2))

let rec natmul (n1, n2) = 
	match (n1, n2) with
	| (ZERO, _) -> ZERO
	| (SUCC(n3), _) -> natadd (n2, natmul(n3, n2))