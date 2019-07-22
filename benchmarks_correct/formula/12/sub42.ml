type formula =
	TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr
and expr =
	NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec eval formula =
	match formula with
	TRUE -> true
	| FALSE -> false
	| NOT formula -> not (eval formula)
	| ANDALSO (formula1, formula2) -> (eval formula1) && (eval formula2)
 	| ORELSE (formula1, formula2) -> (eval formula1) || (eval formula2)
	| IMPLY (formula1, formula2) -> (not (eval formula1) ) ||  (eval formula2)
	| LESS (expr1, expr2) ->
		let rec value expr =
			match expr with
			NUM n -> n
			| PLUS (expr1, expr2) -> (value expr1) + (value expr2)
			| MINUS (expr1, expr2) -> (value expr1) - (value expr2)
		in
		(value expr1) < (value expr2)

		
