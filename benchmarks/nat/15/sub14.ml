(* Ex 5. Natural number *)
type nat = ZERO
		 | SUCC of nat

let rec natadd (n1, n2) =
	match n2 with
	| ZERO -> n1
	| SUCC n2' -> natadd (SUCC n1, n2')

let rec natmul (n1, n2) =
	if (n1 == ZERO) then ZERO
	else
		(match n2 with
		| ZERO -> ZERO
		| SUCC ZERO -> n1
		| SUCC n2' -> natadd(n1, natmul(n1, n2')))

(*
(* using test *)
let rec nat_to_int n =
	match n with
	| ZERO -> 0
	| SUCC n' -> 1 + (nat_to_int n')

let _ =
	let msg = string_of_int (nat_to_int ZERO) in
	print_endline msg

let _ =
	let msg = string_of_int (nat_to_int (SUCC (SUCC ZERO))) in
	print_endline msg

let _ =
	let msg = string_of_int (nat_to_int (natmul ((SUCC (SUCC ZERO)), ZERO))) in
	print_endline msg

let _ =
	let msg = string_of_int (nat_to_int (natmul (ZERO, (SUCC (SUCC ZERO))))) in
	print_endline msg

let _ =
	let msg = string_of_int (nat_to_int (natmul ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO)))))) in
	print_endline msg
*)