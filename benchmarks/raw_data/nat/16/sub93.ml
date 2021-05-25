type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> match n2 with
| ZERO -> n1
| SUCC(tl) -> 
if tl = ZERO then SUCC n1
else natadd (SUCC(n1)) tl;;

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
match n2 with
| ZERO -> ZERO
| SUCC(ZERO) -> n1
| SUCC(tl) -> natadd (natmul n1 tl) n1;;
