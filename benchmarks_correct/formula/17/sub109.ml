(*Computer Science Engineering 2015-12683 Kim Jaein*)
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

let rec toint (value:expr) = 
	match value with
	|NUM i -> i
	|PLUS (x, y) -> (toint x) + (toint y)
	|MINUS (x, y) -> (toint x) - (toint y)

let rec eval (value:formula) = 
	match value with
	|TRUE -> true
	|FALSE -> false
	|NOT negation -> not (eval negation)
	|ANDALSO (a, b) -> (eval a) && (eval b)
	|ORELSE (a, b) -> (eval a) || (eval b)
	|IMPLY (a, b) -> (not (eval a)) || (eval b)
	|LESS (a, b) -> (toint a) < (toint b)

