
type nat = ZERO | SUCC of nat;;

let rec natadd (nat1, nat2) =
	match (nat1, nat2) with
	|	(ZERO, n) | (n, ZERO) -> n
	|	(n1, SUCC n2_1) -> SUCC (natadd (n1, n2_1))
	;;

let rec natmul (nat1, nat2) =
	match (nat1, nat2) with
	|	(ZERO, n) | (n, ZERO) -> ZERO
	|	(n1, SUCC n2) -> natadd(n1, natmul(n1, n2))
	;;

(* exercise test
exercise *)
