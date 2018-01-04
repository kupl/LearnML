(* hw 1_6. *)
type nat = ZERO | SUCC of nat
let rec natadd (n1, n2) =
	match (n1, n2) with
	(ZERO, _) -> n2
	|(SUCC n, _) -> natadd(n, SUCC n2)
let rec natmul (n1, n2) =
	match (n1, n2) with
	(ZERO, _) -> ZERO
	|(_, ZERO) -> ZERO
	|(SUCC ZERO, _) -> n2
	|(SUCC n, _) -> natadd(natmul(n, n2), n2)
