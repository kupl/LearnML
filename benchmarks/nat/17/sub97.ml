(* 2014-15113 Kim MinJI *)

type nat = ZERO | SUCC of nat

let rec natadd (n1: nat) (n2: nat): nat =
	match n1 with
	| ZERO -> n2
	| SUCC n -> natadd n (SUCC n2)

let rec natmul (n1: nat) (n2: nat): nat =
	match (n1, n2) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC n, _) -> natadd (natmul n n2) n2
