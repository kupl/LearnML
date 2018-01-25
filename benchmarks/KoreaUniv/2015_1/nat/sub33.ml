type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
=fun a b ->(* TODO *)
let rec natadd a, b =
	match b with
	| ZERO ->
		let rec renat n =
		match n with
		| ZERO -> ZERO
		| SUCC (n2) -> SUCC (renat n2)
		in
		renat a
	|SUCC (b2) -> SUCC ( natadd a b2 );;



let rec natmul : nat -> nat -> nat
=fun a b ->
	match b with
	| ZERO -> ZERO
	| SUCC (b2) -> natadd a (natmul a, b2);


