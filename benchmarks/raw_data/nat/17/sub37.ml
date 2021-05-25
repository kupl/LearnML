(* ------------------problem2------------------ *)
type nat = ZERO | SUCC of nat
let two = SUCC (SUCC ZERO);;
let three = SUCC (SUCC (SUCC ZERO));;

let natadd : nat -> nat -> nat 
= fun n1 n2 -> 
	let rec add nat1 nat2 = 
		match nat2 with
		| ZERO -> nat1
		| SUCC(a) -> add (SUCC(nat1)) a in
	match n2 with
	| ZERO -> n1
	| SUCC(a) -> add n1 n2;;

let natmul : nat -> nat -> nat 
= fun n1 n2 -> 
	let tmp = n1 in
	let rec mul nat1 nat2 = 
		match nat2 with
		| ZERO -> nat1
		| SUCC(a) -> mul (natadd nat1 tmp) a in
	match n2 with
	| ZERO -> ZERO
	| SUCC(a) -> mul n1 a;;


let natexp : nat -> nat -> nat 
= fun n1 n2 -> 
	let tmp = n1 in
	let rec exp nat1 nat2 = 
		match nat2 with
		| ZERO -> nat1
		| SUCC(a) -> exp (natmul nat1 tmp) a in
	match n2 with
	| ZERO -> SUCC ZERO
	| SUCC(a) -> exp n1 a;;