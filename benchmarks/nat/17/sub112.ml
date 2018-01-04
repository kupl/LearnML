 type nat = ZERO
  		  | SUCC of nat

let rec natadd ((a : nat), (b : nat)) : nat =
	match (a, b) with
	| (ZERO, _) -> b
	| (_, ZERO) -> a
	| (SUCC(am), b) -> natadd(am, SUCC(b))

let rec natmul ((a : nat), (b : nat)) : nat =
	match (a, b) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC(am), b) -> natadd(b, natmul(am, b))
