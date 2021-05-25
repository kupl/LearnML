type formula = True
	|False
	|Not of formula
	|AndAlso of formula * formula
	|OrElse of formula * formula
	|Imply of formula * formula
	|Equal of exp * exp
and exp = Num of int
	|Plus of exp * exp
	|Minus of exp * exp

let rec evalexp e =
	match e with
	|Num i -> i
	|Plus (e1,e2) -> (evalexp e1) + (evalexp e2)
	|Minus (e1,e2) -> (evalexp e1) - (evalexp e2)

let rec eval f =
	match f with
	|True -> true
	|False -> false
	|Not f -> not (eval f)
	|AndAlso (f1,f2) -> if ((eval f1) = true) && ((eval f2) = true) then true
			     else false
	|OrElse (f1,f2) -> if ((eval f1) = true) || ((eval f2) = true) then true
			   else false
	|Imply (f1,f2) -> if ((eval f1) = true) && ((eval f2) = false) then false
			  else true
	|Equal (e1,e2) -> if (evalexp e1) = (evalexp e2) then true
			 else false
