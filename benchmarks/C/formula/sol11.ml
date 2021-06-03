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


let rec eval f =
	let rec chupa exp = 
		match exp with
		Num i -> i
		|Plus (ex1, ex2) ->
				chupa(ex1) + chupa(ex2)
		|Minus (ex1, ex2) ->
				chupa(ex1) - chupa(ex2)

		in
	
	
			match f with
			True -> true
			|False -> false
			|Not(forma) ->
					if eval(forma) = true then false
					else true
			|AndAlso (forma1, forma2) ->
					if eval(forma1) = true && eval(forma2) = true then true
					else false
			|OrElse (forma1, forma2) ->
					if eval(forma1) = true || eval(forma2) = true then true
					else false
			|Imply (forma1, forma2) ->
					if eval(forma1) = true && eval(forma2) = false then false
					else true
			|Equal (ex1, ex2) ->
						if chupa(ex1) = chupa(ex2) then true
						else false

