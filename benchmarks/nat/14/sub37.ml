type nat = ZERO | SUCC of nat



let rec natadd ((na : nat), (nb : nat)) : nat =
	match na with
	| ZERO -> nb
	| SUCC a -> natadd (a, SUCC nb)