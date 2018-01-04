(* 컴퓨터공학부/2009-11679/김정명/6 *)

type nat = ZERO | SUCC of nat

let rec natadd (n1, n2) =
	match n1 with
	  ZERO -> n2
	| SUCC n -> SUCC (natadd (n, n2))
;;

let natmul (n1, n2) =
	let rec iter (a, b, o) =
		match a with
		  ZERO -> ZERO
		| SUCC ZERO -> b
		| SUCC n -> iter (n, natadd(o, b), o)
	in
	iter (n1, n2, n2)
;;
