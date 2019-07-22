(* 2009-11824 Jieun-Jeong HW1-5 *)

type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec eval f =
	let rec calc e =
		match e with
		NUM n		-> n
		|PLUS (el, er)	-> (calc el) + (calc er)
		|MINUS (el, er) -> (calc el) - (calc er)
	in
	match f with
	TRUE			-> true
	|FALSE			-> false
	|NOT f			-> if (eval f) then false else true
	|ANDALSO (fl, fr)	-> (eval fl) && (eval fr)
	|ORELSE (fl, fr)	-> (eval fl) || (eval fr)
	|IMPLY (fl, fr)		-> (eval (NOT fl)) || (eval fr)
	|LESS (el, er)		-> (calc el) < (calc er)	
