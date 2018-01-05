(* 2008-11874 Lee, Sujee *)
(* EXERCISE 6 *)
type nat = ZERO | SUCC of nat

let rec natadd(a,b) = (* natadd : nat * nat -> nat = <fun> *)
	match (a,b) with
		| (SUCC nat1,nat2) -> SUCC(natadd(nat1,nat2))
		| (ZERO,SUCC nat2) -> SUCC(natadd(ZERO,nat2))
		| (ZERO,ZERO) -> ZERO
	
let rec natmul(a,b) = (* natmul : nat * nat -> nat = <fun> *)
	match (a,b) with
		| (SUCC nat1, nat2) -> natadd((natmul(nat1,nat2),nat2))
		| (ZERO, nat2) -> ZERO

(*
let result6 = natadd(SUCC(SUCC(SUCC(ZERO))),SUCC(ZERO))
let result62 = natmul(SUCC(SUCC(SUCC(ZERO))),SUCC(ZERO))
let _ =
	print_string "EXERCISE 6 : ";
	print_string result6;
	print_newline();
	print_string result62;
	print_newline()*)