
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

let rec calexp exp =
	match exp with
	|Num a -> a
	|Plus(a,b) -> calexp(a) + calexp(b)
	|Minus(a,b) -> calexp(a) - calexp(b)

let rec eval formula = 
	match formula with
	|True -> true
	|False -> false
	|Not f1 -> not(eval f1)
	|AndAlso(f1, f2) -> (eval f1) && (eval f2)
	|OrElse(f1, f2) -> (eval f1) || (eval f2)
	|Imply(f1, f2) -> not(eval f1) || (eval f2)
	|Equal(e1, e2) -> calexp(e1) = calexp(e2)
