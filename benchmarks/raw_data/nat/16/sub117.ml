type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 ->
match n2 with
ZERO -> n1
|SUCC(x) -> natadd (SUCC(n1)) x;;


let rec natmul : nat -> nat -> nat
= fun n1 n2 -> 
let rec loopsum : nat -> nat -> nat
= fun s x ->
match x with
ZERO -> s
|SUCC(x) -> loopsum (natadd s n1) x in
loopsum ZERO n2;;
