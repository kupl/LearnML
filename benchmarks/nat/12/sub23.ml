(* HW1 exercise7 2009-11697 Kim HyunJoon *)
(* Natural number *)

type nat = ZERO | SUCC of nat

let rec natadd : nat * nat -> nat =
	fun (n1, n2) ->
	match (n1, n2) with
	| (ZERO, _) -> n2
	| (_, ZERO) -> n1
	| (SUCC m, _) -> (natadd (m, (SUCC n2)))

let rec natmul : nat * nat -> nat =
	fun (n1, n2) ->
	match (n1, n2) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC m, _) -> (natadd (n2, (natmul (m, n2))))
	 
