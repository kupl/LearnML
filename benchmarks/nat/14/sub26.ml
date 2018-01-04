(* College of Liberal Studies 2010-13342 Kim Ye Jung *)
(* 2014.2 Programming Languages Homework 1 - 3 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat =
	fun (a, b) ->
	match a with
	| SUCC n -> natadd(n,SUCC b)
	| ZERO -> b
	
let rec natmul : nat * nat -> nat =
	fun (a, b) ->
	match a with
	| SUCC n -> natadd(b, natmul(n,b))
	| ZERO -> ZERO
