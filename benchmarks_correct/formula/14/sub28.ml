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

let rec cal n =
	match n with
		Num x -> x
		|Plus(x, y) -> cal x + cal y
		|Minus(x, y) -> cal x - cal y


let rec eval b =
	match b with
		True -> true
		| False -> false
		| Not x -> 
			if eval x = true then false
			else true
		| AndAlso (x, y) ->
			if eval x = true && eval y = true then true
			else false
		| OrElse (x, y) ->
			if eval x = true || eval y = true then true
			else false
		| Imply (x, y) ->
			if eval x = true && eval y = false then false
			else true
		| Equal (x, y) ->
			if cal x = cal y then true
			else false

