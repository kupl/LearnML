(*
 * Dept of Physics Education
 * 2012-12666 Choi Jaehyeok
 * Homework 1, Problem 4
 *)

type nat = ZERO | SUCC of nat

let rec natadd : (nat * nat) -> nat = fun (a, b) ->
	match (a, b) with
	| (ZERO, _) -> b
	| (_, ZERO) -> a
	| (SUCC i, _) -> SUCC (natadd (i, b))
	
let rec natmul : nat * nat -> nat = fun (a, b) ->
	match (a, b) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (_, SUCC i) -> natadd (a, natmul (a, i))
