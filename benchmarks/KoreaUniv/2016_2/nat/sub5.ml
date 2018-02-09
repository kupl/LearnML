type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n1 with ZERO -> n2 | SUCC(n0) -> natadd n0 (SUCC(n2));;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> let rec overload : nat -> nat -> nat -> nat = fun n1 n2 k -> match n1 with ZERO -> ZERO | SUCC(ZERO) -> n2 | SUCC(n0) -> overload n0 (natadd n2 k ) k in overload n1 n2 n2;;
