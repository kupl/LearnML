type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat
= fun n1 n2 -> 
	let rec nataddrec n1 n2 t =
		if t = n2 then n1 else SUCC (nataddrec n1 n2 (SUCC t))
	in nataddrec n1 n2 ZERO;;

let natmul : nat -> nat -> nat
= fun n1 n2 -> 
	let rec natmultrec n1 n2 t =
		if t = n2 then ZERO else natadd (natmultrec n1 n2 (SUCC t)) n1
	in natmultrec n1 n2 ZERO;;
