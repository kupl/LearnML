type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
match n1 with
ZERO -> n2
|SUCC(sth) -> SUCC( natadd sth n2 )

let rec natmul : nat -> nat -> nat
= fun n1 n2 -> (* TODO *)
match n1 with
ZERO -> ZERO
|SUCC(sth) -> natadd n2 (natmul sth n2)
