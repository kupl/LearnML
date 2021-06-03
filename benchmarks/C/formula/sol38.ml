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

let rec eval form =
	let rec cals exp =
		match exp with
		(Num a) ->
			a
		|(Plus (a,b)) ->
			((cals a) + (cals b))
		|(Minus (a,b)) ->
			((cals a) - (cals b))
	in
	match form with
	True -> 
		true
	|False -> 
		false
	|(Not a) ->
		(not (eval a))
	|(AndAlso (a,b)) ->
		(if (((eval a) = true) && ((eval b) = true)) then
			true
		else
			false
		)
	|(OrElse (a,b)) ->
		(if (((eval a) = true) || ((eval b) = true)) then
		 	true
		else
			false
		)
	|(Imply (a,b)) ->
		(if (((eval a) = false) || ((eval b) = true)) then
		 	true
		else
			false
		)
	|(Equal (a,b)) ->
		((cals a) = (cals b))


