
(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
	match (n1,n2) with
	| (n1,ZERO) -> n1
	| (ZERO,n2) -> n2
	| (SUCC (n3), SUCC (n4)) -> natadd (SUCC (SUCC (n3))) n4;;


let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
	match (n1,n2) with
	| (n1,ZERO) -> ZERO
	| (ZERO,n2) -> ZERO
	| (n1, SUCC ZERO) -> n1
	| (SUCC ZERO, n2) -> n2
	| (SUCC (n3), SUCC (n4)) -> natadd (natmul (SUCC (n3)) (n4)) (SUCC (n3));;
 

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
	match (n1,n2) with
	| (n1, ZERO) -> SUCC ZERO
	| (n1, SUCC ZERO) -> n1
	| (n1, SUCC (n3)) -> natmul (natexp n1 n3) n1;;

