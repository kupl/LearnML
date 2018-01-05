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

let rec makeint ex =
	match ex with
	| NUM i -> i
	| PLUS (lex1, rex1) -> makeint lex1 + makeint rex1
	| MINUS (lex2, rex2) -> makeint lex2 - makeint rex2



let rec eval form1 =
	match form1 with
	| TRUE -> true
	| FALSE -> false
	| NOT form1'  -> not (eval form1')
	| ANDALSO (f1, f2) -> (eval f1) && (eval f2)
	| ORELSE (f3, f4) -> (eval f3) || (eval f4)
	| IMPLY (f5, f6) -> not (eval f5) || (eval f6)
	| LESS (ex1, ex2) -> if (makeint ex1)<(makeint ex2) then true else false
