(* hw1-7 *)
(* 2010-11687 Keunjun Choi *)

type nat = ZERO | SUCC of nat
let rec natadd (a, b) =
	match (a, b) with
	| (ZERO, _) -> b
	| (_, ZERO) -> a
	| (SUCC (a1),SUCC (b1)) -> SUCC (SUCC (natadd (a1, b1)))
let rec natmul (a, b) =
	match (a, b) with
	| (ZERO, _) -> ZERO
	| (_, ZERO) -> ZERO
	| (SUCC (a1),SUCC (b1)) -> SUCC (natadd (natmul (a1, b1), natadd (a1, b1)))
