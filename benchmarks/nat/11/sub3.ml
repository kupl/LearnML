(* 200511843 LEE JONGHO *)

type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match n1 with
	ZERO -> n2
	| SUCC n1' -> natadd (n1', SUCC n2)

let natmul (n1, n2) =
	if (n1 = ZERO || n2 = ZERO) then ZERO
	else
		let rec natmul_in n1 n2 s =
			match n1 with
			ZERO -> s 
			| SUCC n1' -> natmul_in n1' n2 (natadd (n2, s))
		in
			natmul_in n1 n2 ZERO
