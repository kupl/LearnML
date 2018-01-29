(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
match n1 with
| ZERO -> n2
| SUCC a -> natadd a (SUCC n2)

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
	let rec natmul_tmp : nat->nat->nat->nat
	= fun n1_origin n1 n2->
		match n2 with
		|ZERO -> ZERO
		|SUCC ZERO -> n1
		|SUCC a -> natmul_tmp n1_origin (natadd n1_origin n1) a
	in
natmul_tmp n1 n1 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> 
	let rec natexp_tmp : nat->nat->nat->nat
	= fun n1_origin n1 n2->
		match n2 with
		|ZERO -> SUCC ZERO
		|SUCC ZERO -> n1
		|SUCC a -> natexp_tmp n1_origin (natmul n1_origin n1) a
	in
natexp_tmp n1 n1 n2