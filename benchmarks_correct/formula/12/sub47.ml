type formula = 	 
	 TRUE
	|FALSE
	|NOT of formula
	|ANDALSO of formula * formula
	|ORELSE of formula * formula
	|IMPLY of formula * formula
	|LESS of expr * expr

and expr = 
	 NUM of int
	|PLUS of expr * expr
	|MINUS of expr * expr

let rec doexpr (x:expr) =
	match x with
	|NUM a -> a
	|PLUS (a, b) -> (doexpr a) + (doexpr b)
	|MINUS (a, b) -> (doexpr a) - (doexpr b)

let rec eval (form:formula) = 
	match form with
	|TRUE -> true
	|FALSE -> false
	|NOT a -> not (eval a)
	|ANDALSO (a, b) -> (eval a) && (eval b)
	|ORELSE (a, b) -> (eval a) || (eval b) 
	|IMPLY (a, b) -> if (eval a) then (eval b) else true
	|LESS (a, b) -> if (doexpr a) < (doexpr b) then true else false 
