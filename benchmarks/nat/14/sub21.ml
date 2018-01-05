(*
** PL::HW[01].Problem[03]
** 
** Last Mod.: 2014-09-14 21:36
** Writ. by : CMS
*)

type nat = ZERO | SUCC of nat

let rec natadd (a, b) = 
	match b with
	| ZERO -> a
	| SUCC s -> natadd (SUCC a, s)

let rec natmul (a, b) =
	if a = ZERO then ZERO else
	if b = ZERO then ZERO else
	match b with
	| ZERO -> a
	| SUCC s -> natadd (natmul (a, s), a)

