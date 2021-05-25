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


let rec eval form =
	let rec calc e =
		match e with
		Num i -> i
		|Plus(i1, i2) -> calc(i1) + calc(i2)
		|Minus(i1, i2) -> calc(i1) - calc(i2) in
	match form with
	True -> true
	|False -> false
	|Not f ->
		if eval(f) = true then false
		else true
	|AndAlso(f1, f2) ->
		if eval(f1) = true && eval(f2) = true then true
		else false
	|OrElse(f1, f2) ->
		if eval(f1) = true || eval(f2) = true then true
		else false
	|Imply(f1, f2) ->
		if eval(f1) = false || eval(f2) = true then true
		else false
	|Equal(e1, e2) ->
		if calc(e1) = calc(e2) then true
		else false
