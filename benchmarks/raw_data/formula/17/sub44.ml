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

let rec calc = fun exp ->
	match exp with
	|Num i -> i
	|Plus (lexp, rexp) -> (calc lexp) + (calc rexp)
	|Minus (lexp, rexp) -> (calc lexp) - (calc rexp)
let rec eval = fun formula ->
	match formula with
	|True -> true
	|False -> false
	|Not form -> 
		(match (eval form) with
		|true -> false
		|false -> true)
	|AndAlso (lform, rform) ->
		(match (eval lform) with
		|false -> false
		|true -> (eval rform))
	|OrElse (lform, rform) ->
		(match (eval lform) with
		|true-> true
		|false -> (eval rform))
	|Imply (lform, rform) ->
		(match (eval lform) with
		|false -> true
		|true -> (eval rform))
	|Equal (lex, rex) -> 
		if ((calc lex) = (calc rex)) then true
		else false
