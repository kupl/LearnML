type nat = ZERO | SUCC of nat;;

let rec natadd ((n1 : nat), (n2 : nat)) : nat = 
	match n2 with
	| ZERO -> n1
	| SUCC n3 -> natadd(SUCC n1, n3)
;;

let rec natmul ((n1 : nat), (n2 : nat)) : nat = 
	match (n1, n2) with
	| (_, ZERO) | (ZERO, _) -> ZERO
	| (SUCC n3, _) -> natadd(natmul(n3, n2), n2)
;;
