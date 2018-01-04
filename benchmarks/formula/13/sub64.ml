type formula 	= TRUE
		| FALSE
		| NOT of formula
		| ANDALSO of formula * formula
		| ORELSE of formula * formula
		| IMPLY of formula * formula
		| LESS of expr * expr
and expr 	= NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr


let rec eval form =
	match form with
	|TRUE -> true
	| FALSE -> false
	| NOT form1 -> not (eval form1)
	| ANDALSO (form1, form2) -> ((eval form1) && (eval form2))
	| ORELSE (form1, form2) -> ((eval form1) || (eval form2))
	| IMPLY (form1, form2) -> not ((eval form1) && (not (eval form2)))
	| LESS (expr1, expr2) -> ((calc expr1) < (calc expr2))


and calc exp =
	match exp with
	|NUM ex -> ex
	| PLUS(ex1, ex2) -> ((calc ex1) + (calc ex2))
	| MINUS(ex1, ex2) -> ((calc ex1) - (calc ex2))
 

