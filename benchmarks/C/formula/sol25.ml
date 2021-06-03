exception Error of string
type formula = 
	True 
	|False
	|Not of formula
	|AndAlso of formula * formula
	|OrElse of formula * formula
	|Imply of formula * formula
	|Equal of exp * exp
	and exp = Num of int |Plus of exp * exp|Minus of exp * exp
let rec calc : exp -> int =
	(function myexp ->
		(match myexp with
			Num a -> a
			|Plus (a1,a2) -> ((calc a1) + (calc a2))
			|Minus (b1,b2) -> ((calc b1) - (calc b2))))

let rec eval_temp : formula -> bool =
	(function formu ->
		(match formu with
			True -> true
			|False -> false
			|Not a -> (not (eval_temp a))
			|AndAlso (a1,a2) -> ((eval_temp a1) && (eval_temp a2))
			|OrElse (b1,b2) -> ((eval_temp b1) || (eval_temp b2))
			|Imply (c1,c2) -> (not ((eval_temp c1) && (not (eval_temp c2))))
			|Equal (ex1,ex2) -> ((calc ex1) = (calc ex2))));;
let eval : formula -> bool =
		(function formu -> (eval_temp formu))
		
