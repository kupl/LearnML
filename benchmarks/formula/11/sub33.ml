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
	let rec evalexp ex =
		match ex with
		NUM x -> x
		| PLUS (x, y) -> (evalexp(x)+evalexp(y))
		| MINUS (x, y) -> (evalexp(x)-evalexp(y))
	in
	match f with
	TRUE -> true
	| FALSE -> false
	| NOT x -> not(eval(x))
	| ANDALSO (x, y) -> (eval(x) & eval(y))
	| ORELSE (x, y) -> (eval(x) or eval(y))
	| IMPLY (x, y) -> (not(eval(x)) or eval(y))
	| LESS (x, y) -> (evalexp(x) < evalexp(y))
