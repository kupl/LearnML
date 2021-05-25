type formula 	= True
		| False
		| Not of formula
		| AndAlso of formula * formula
		| OrElse of formula * formula
		| Imply of formula * formula
		| Equal of exp * exp
and exp 	= Num of int
		| Plus of exp * exp
		| Minus of exp * exp


let rec eval form =
	match form with
	|True -> true
	| False -> false
	| Not form1 -> not (eval form1)
	| AndAlso (form1, form2) -> ((eval form1) && (eval form2))
	| OrElse (form1, form2) -> ((eval form1) || (eval form2))
	| Imply (form1, form2) -> not ((eval form1) && (not (eval form2)))
	| Equal (exp1, exp2) -> ((calc exp1) = (calc exp2))


and calc exp =
	match exp with
	|Num ex -> ex
	| Plus(ex1, ex2) -> ((calc ex1) + (calc ex2))
	| Minus(ex1, ex2) -> ((calc ex1) - (calc ex2))
 

