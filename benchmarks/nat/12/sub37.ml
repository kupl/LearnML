type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match (n1, n2) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, s2) -> s2
	| (s1, ZERO) -> s1
	| (SUCC s1, s2) -> (SUCC(natadd (s1, s2)))
;;

let rec natmul (n1, n2) =
	match (n1, n2) with
	| (ZERO, ZERO) -> ZERO
	| (ZERO, s2) -> ZERO
	| (s1, ZERO) -> ZERO
	| (SUCC s1, s2) -> natadd(natmul(s1,s2),s2)
;;