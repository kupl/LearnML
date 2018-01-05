(*Seok Jin Lee 2013-11417 CSE*)

type formula 	= TRUE
		| FALSE
		| NOT of formula
		| ANDALSO of formula * formula
		| ORELSE of formula * formula
		| IMPLY of formula * formula
		| LESS of expr * expr

and expr	= NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr

(*
let rec evalexpr(exp: expr): int = 
	match exp with
	| NUM n -> n
	| PLUS(l,r) -> evalexpr(l) + evalexpr(r)
	| MINUS(l,r) -> evalexpr(l) - evalexpr(r)
*)

let rec eval(f: formula): bool =
	let rec evalexpr(exp: expr): int = 
		match exp with
		| NUM n -> n
		| PLUS(l,r) -> evalexpr(l) + evalexpr(r)
		| MINUS(l,r) -> evalexpr(l) - evalexpr(r)
	
	in 
	match f with
	| TRUE -> true
	| FALSE -> false
	| NOT op -> not(eval(op))
	| ANDALSO(l,r) -> eval(l) && eval(r)
	| ORELSE(l,r) -> eval(l) || eval(r)
	| IMPLY(l,r) -> not(eval(l)) || eval(r)
	| LESS(l,r) -> evalexpr(l) < evalexpr(r) 


