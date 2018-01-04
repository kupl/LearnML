(* not tested *)

type nat = ZERO | SUCC of nat

let rec natadd ((n1: nat), (n2: nat)) =
	match (n1, n2) with
	| (ZERO, _) -> n2
	| (_, ZERO) -> n1
	| (n1, (SUCC s2)) -> SUCC (natadd (n1, s2))

let natmul ((n1: nat), (n2: nat)) = 
	let rec natmul_sub ((n1: nat), (n2: nat), (sum: nat)) = 
		match (n1, n2) with
		| (ZERO, _) -> sum
		| (_, ZERO) -> sum
		| (n1, (SUCC s2)) -> natmul_sub (n1, s2, (natadd (sum, n1)))
	in
	natmul_sub (n1, n2, ZERO)
