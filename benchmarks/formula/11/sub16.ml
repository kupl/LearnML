
(* 2008-11720 Á¶°Ü¸® *)

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


let rec eval form = 
	let rec exval exp =
		match exp with
		NUM a -> a
		| PLUS (e1, e2) -> (exval e1)+(exval e2)
		| MINUS (e1, e2) -> (exval e1)-(exval e2)
	in

	match form with
	TRUE -> true
	| FALSE -> false
	| NOT f -> (not (eval f))
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f1, f2) -> (eval f1) || (eval f2)
	| IMPLY (f1, f2) -> (not (eval f1)) || (eval f2)
	| LESS (e1, e2) -> if (exval e1)<(exval e2) then true
						else false
