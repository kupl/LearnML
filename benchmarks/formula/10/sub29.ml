(*hw1-5 컴퓨터 공학부 2008-11641 신희식*) 

type formula = TRUE
			| FALSE
			| NOT of formula
			| ANDALSO of formula * formula
			| ORELSE of formula * formula
			| IMPLY of formula * formula
			| LESS of expr * expr
and expr = NUM of int
		| PLUS of expr * expr
		| MINUS of expr * expr

let rec eval form =
	let rec cals exp =
		match exp with
		(NUM a) ->
			a
		|(PLUS (a,b)) ->
			((cals a) + (cals b))
		|(MINUS (a,b)) ->
			((cals a) - (cals b))
	in
	match form with
	TRUE -> 
		true
	|FALSE -> 
		false
	|(NOT a) ->
		(not (eval a))
	|(ANDALSO (a,b)) ->
		(if (((eval a) = true) && ((eval b) = true)) then
			true
		else
			false
		)
	|(ORELSE (a,b)) ->
		(if (((eval a) = true) || ((eval b) = true)) then
		 	true
		else
			false
		)
	|(IMPLY (a,b)) ->
		(if (((eval a) = false) || ((eval b) = true)) then
		 	true
		else
			false
		)
	|(LESS (a,b)) ->
		((cals a) < (cals b))


