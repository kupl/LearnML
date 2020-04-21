(*Computer Science Engineering 2015-12683 Kim Jaein*)
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

let rec toint (value:exp) = 
	match value with
	|Num i -> i
	|Plus (x, y) -> (toint x) + (toint y)
	|Minus (x, y) -> (toint x) - (toint y)

let rec eval (value:formula) = 
	match value with
	|True -> true
	|False -> false
	|Not negation -> not (eval negation)
	|AndAlso (a, b) -> (eval a) && (eval b)
	|OrElse (a, b) -> (eval a) || (eval b)
	|Imply (a, b) -> (not (eval a)) || (eval b)
	|Equal (a, b) -> (toint a) = (toint b)

