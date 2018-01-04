(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #5 *)
type nat = ZERO | SUCC of nat

let rec natadd(n1, n2) : nat =
	match n1, n2 with
	| a, ZERO -> a
	| a, SUCC b -> natadd(SUCC a, b)

let rec natmul(n1, n2) : nat =
	match n1, n2 with
	| a, ZERO -> ZERO
	| a, SUCC b -> natadd(a, natmul(a, b))
