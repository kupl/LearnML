type formula = TRUE
				|FALSE
				|NOT of formula
				|ANDALSO of formula * formula
				|ORELSE of formula * formula
				|IMPLY of formula * formula
				|LESS of expr * expr
	and expr = NUM of int
				|PLUS of expr * expr
				|MINUS of expr * expr


let rec eval form =
	let rec calc e =
		match e with
		NUM i -> i
		|PLUS(i1, i2) -> calc(i1) + calc(i2)
		|MINUS(i1, i2) -> calc(i1) - calc(i2) in
	match form with
	TRUE -> true
	|FALSE -> false
	|NOT f ->
		if eval(f) = true then false
		else true
	|ANDALSO(f1, f2) ->
		if eval(f1) = true && eval(f2) = true then true
		else false
	|ORELSE(f1, f2) ->
		if eval(f1) = true || eval(f2) = true then true
		else false
	|IMPLY(f1, f2) ->
		if eval(f1) = false || eval(f2) = true then true
		else false
	|LESS(e1, e2) ->
		if calc(e1) < calc(e2) then true
		else false
