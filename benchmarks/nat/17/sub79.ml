(*
		CSE / 2013-11426 / Im DongYeop
		Homework 1 : Exercise 4
*)

type nat = ZERO | SUCC of nat

let rec natadd((a: nat), (b: nat)): nat =
	match (a, b) with
	| (ZERO, _) -> b
	| (_, ZERO) -> a
	| (SUCC ina, SUCC inb) -> natadd(SUCC a, inb)

let rec natmul((a: nat), (b: nat)): nat =
	match (a, b) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC ina, SUCC inb) -> natadd(a, natmul(a, inb))
