
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calx : exp -> int -> int
  = fun exp int ->
  	match exp with
  		| X ->
  			int
  		| INT (a) ->
  			a
  		| ADD (a, b) ->
  			(calx a int) + (calx b int)
  		| SUB (a, b) ->
  			(calx a int) - (calx b int)
  		| MUL (a, b) ->
  			(calx a int) * (calx b int)
  		| DIV (a, b) ->
  			(calx a int) / (calx b int)
  		| SIGMA (a, b, c) ->
		  	if (calx a 0) != (calx b 0) then
		  		if (calx a 0) < (calx b 0) then
  					(calx (SIGMA(INT((calx a 0) + 1), b, c)) (calx a int)) + (calx c (calx a int))
  				else
  					(calx (SIGMA(INT((calx a 0) - 1), b, c)) (calx a int)) + (calx c (calx a int))
  			else
				calx c (calx a int)
  		|_ ->
			int
  		
  let calculator : exp -> int
  = fun exp ->
  	calx exp 0