exception Error of string

(* EX5 : natadd & natmul *)
type nat = ZERO | SUCC of nat
(* definition of nat *)

let rec natadd a b =
	match a with
		ZERO -> b
		| SUCC x  -> 
			match b with
				ZERO -> SUCC x
				| SUCC y -> natadd ( SUCC ( SUCC x ) ) y

let rec natmul a b =
	match a with
		ZERO -> ZERO
		| SUCC x ->
			match b with
				ZERO -> ZERO
				| SUCC y -> natadd ( SUCC x)  ( natmul ( SUCC x ) y )
