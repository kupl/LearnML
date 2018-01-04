exception Error of string
type formula = 
	TRUE 
	|FALSE
	|NOT of formula
	|ANDALSO of formula * formula
	|ORELSE of formula * formula
	|IMPLY of formula * formula
	|LESS of expr * expr
	and expr = NUM of int |PLUS of expr * expr|MINUS of expr * expr
let rec calc : expr -> int =
	(function myexp ->
		(match myexp with
			NUM a -> a
			|PLUS (a1,a2) -> ((calc a1) + (calc a2))
			|MINUS (b1,b2) -> ((calc b1) - (calc b2))))

let rec eval_temp : formula -> bool =
	(function formu ->
		(match formu with
			TRUE -> true
			|FALSE -> false
			|NOT a -> (not (eval_temp a))
			|ANDALSO (a1,a2) -> ((eval_temp a1) && (eval_temp a2))
			|ORELSE (b1,b2) -> ((eval_temp b1) || (eval_temp b2))
			|IMPLY (c1,c2) -> (not ((eval_temp c1) && (not (eval_temp c2))))
			|LESS (ex1,ex2) -> ((calc ex1) < (calc ex2))));;
let eval : formula -> bool =
	try
		(function formu -> (eval_temp formu))
	with e -> raise (Error "Invalid error.");;
		
