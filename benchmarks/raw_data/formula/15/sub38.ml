type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec makeint ex =
	match ex with
	| Num i -> i
	| Plus (lex1, rex1) -> makeint lex1 + makeint rex1
	| Minus (lex2, rex2) -> makeint lex2 - makeint rex2



let rec eval form1 =
	match form1 with
	| True -> true
	| False -> false
	| Not form1'  -> not (eval form1')
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f3, f4) -> (eval f3) || (eval f4)
	| Imply (f5, f6) -> not (eval f5) || (eval f6)
	| Equal (ex1, ex2) -> if (makeint ex1)=(makeint ex2) then true else false
