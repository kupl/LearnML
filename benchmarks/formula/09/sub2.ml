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


let rec eval f =
	let rec chupa exp = 
		match exp with
		NUM i -> i
		|PLUS (ex1, ex2) ->
				chupa(ex1) + chupa(ex2)
		|MINUS (ex1, ex2) ->
				chupa(ex1) - chupa(ex2)

		in
	
	
			match f with
			TRUE -> true
			|FALSE -> false
			|NOT(forma) ->
					if eval(forma) = true then false
					else true
			|ANDALSO (forma1, forma2) ->
					if eval(forma1) = true && eval(forma2) = true then true
					else false
			|ORELSE (forma1, forma2) ->
					if eval(forma1) = true || eval(forma2) = true then true
					else false
			|IMPLY (forma1, forma2) ->
					if eval(forma1) = true && eval(forma2) = false then false
					else true
			|LESS (ex1, ex2) ->
						if chupa(ex1) < chupa(ex2) then true
						else false

