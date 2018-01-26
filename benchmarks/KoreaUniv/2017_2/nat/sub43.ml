(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
	let rec sub a b =
		match a with
		| ZERO -> b
		| SUCC c -> SUCC(sub c b) 
	in sub n1 n2

let natmul : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
	let rec sub a b ans =
		match a with
		| ZERO -> ans
		| SUCC c -> sub c b (natadd ans b)
	in sub n1 n2 ZERO 

let natexp : nat -> nat -> nat 
= fun n1 n2 -> (* TODO *)
	let rec sub a b ans =
		match b with
		| ZERO -> ans
		| SUCC c -> sub a c (natmul ans a)
	in sub n1 n2 (SUCC ZERO)
