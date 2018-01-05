(* file name : ex3.ml *)
(* author : Jisoon Park (jspark@ropas.snu.ac.kr) *)
(* date : 2013-09-13 *)
(* Exercise 3 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat
 = fun (n1, n2) -> 
	match(n1, n2) with
	| (_, ZERO) -> n1
	| (_, SUCC na2) -> natadd (SUCC n1, na2)

let rec natmul : nat * nat -> nat
 = fun (n1, n2) ->
	match (n1, n2) with
	| (ZERO, _) | (_, ZERO) -> ZERO
	| (_, SUCC na2) -> natadd(natmul (n1, na2), n1)
	
