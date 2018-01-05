(*
	department : computer science & engineering
	student ID : 2012-11242 / name : Seon-bi, Park
*)

type nat = ZERO | SUCC of nat

let natadd (lnat, rnat) = 						(* nat * nat -> nat *) 
	match (lnat, rnat) with
		| (ZERO, ZERO) -> ZERO
		| (ZERO, SUCC(natval)) -> rnat
		| (SUCC(natval), ZERO) -> lnat
		| (SUCC(natlval), SUCC(natrval)) -> SUCC( SUCC( natadd(natlval, natrval)) )

let rec natmul (lnat, rnat) =                       (* nat * nat -> nat *)
	match (lnat, rnat) with
		| (ZERO, ZERO) -> ZERO
		| (ZERO, SUCC(natval)) -> ZERO
		| (SUCC(natval), ZERO) -> ZERO
		| (SUCC(natlval), SUCC(natrval)) -> natadd( natmul( natlval, rnat), rnat)
