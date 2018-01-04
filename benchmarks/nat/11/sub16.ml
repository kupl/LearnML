
(* 2008-11720 Á¶°Ü¸® *)

type nat = ZERO | SUCC of nat

let rec natadd (a, b) =
	match a with
	ZERO -> b
	| SUCC x -> (natadd (x, (SUCC b))) 

let natmul (a, b) =
	let rec inmul a b r=
	match a with
	ZERO -> r
	| SUCC x -> (inmul x b (natadd (b, r)))
	in
	inmul a b ZERO
