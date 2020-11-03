type formula = 	 
	 True
	|False
	|Not of formula
	|AndAlso of formula * formula
	|OrElse of formula * formula
	|Imply of formula * formula
	|Equal of exp * exp

and exp = 
	 Num of int
	|Plus of exp * exp
	|Minus of exp * exp

let rec doexp (x:exp) =
	match x with
	|Num a -> a
	|Plus (a, b) -> (doexp a) + (doexp b)
	|Minus (a, b) -> (doexp a) - (doexp b)

let rec eval (form:formula) = 
	match form with
	|True -> true
	|False -> false
	|Not a -> not (eval a)
	|AndAlso (a, b) -> (eval a) && (eval b)
	|OrElse (a, b) -> (eval a) || (eval b) 
	|Imply (a, b) -> if (eval a) then (eval b) else true
	|Equal (a, b) -> if (doexp a) = (doexp b) then true else false 
