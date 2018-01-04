(* 2004-11957 "Computer science and engineering" "Park Kwang-seok" homework#1-6 *)

type nat = ZERO | SUCC of nat

(* a+b = (a-1) + (b+1) *)
let rec natadd (a, b) = match (a, b) with (ZERO, _) -> b
		| (_, ZERO) -> a
		| (SUCC x, SUCC y) -> natadd (x, (SUCC (SUCC y)))

(* ab = (a-1)b + b *)
let rec natmul (a, b) = match (a, b) with (ZERO, _) -> ZERO
		| (_, ZERO) -> ZERO
		| (SUCC x, y) -> natadd (natmul (x, y), y)

(* test code *)

