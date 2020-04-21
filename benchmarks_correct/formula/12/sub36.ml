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


let rec calc exp =
	match exp with
	| Num exp -> exp
	| Plus(expa, expb) -> (calc expa) + (calc expb)
	| Minus(expa, expb) -> (calc expa) - (calc expb)

let rec eval exp = 
	match exp with 
	| True -> true
	| False -> false
	| Not expa -> not (eval expa)
	| AndAlso(expa, expb) -> ( (eval expa) && (eval expb) )
	| OrElse(expa, expb) -> ( (eval expa) || (eval expb) )
	| Imply(expa, expb) -> ( (not (eval expa) ) || (eval expb) )
	| Equal(expa, expb) -> 
		if (calc expa) = (calc expb) then true
		else false
