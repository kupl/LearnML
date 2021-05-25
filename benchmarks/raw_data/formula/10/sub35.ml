type exp = Num of int | Plus of exp * exp | Minus of exp * exp
type formula = True | False | Not of formula | AndAlso of formula * formula | OrElse of formula * formula | Imply of formula * formula | Equal of exp * exp

let rec eval form = 
	let rec getValue exp = 
		match exp with
		Num n -> n
		|Plus  (a, b) -> (getValue a) + (getValue b)
		|Minus (a, b) -> (getValue a) - (getValue b)
	in
	match form with
	True -> true
	|False -> false
	|Not a -> if (eval a) then false 
		  else true
	|AndAlso (a, b) -> (eval a) && (eval b)
	|OrElse (a,b) -> (eval a) || (eval b)
	|Imply (a,b) ->
		if ((eval a) && (eval (Not b))) then false
		else true
	|Equal (a,b) ->
		if ((getValue a) = (getValue b)) then true
		else false
	|_ -> true
	
		
