
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calx : exp -> int -> int
  = fun exp n ->
  	match exp with
  		| X ->
  			n
  		| INT (a) ->
  			a
  		| ADD (a, b) ->
  			(calx a n) + (calx b n)
  		| SUB (a, b) ->
  			(calx a n) - (calx b n)
  		| MUL (a, b) ->
  			(calx a n) * (calx b n)
  		| DIV (a, b) ->
  			(calx a n) / (calx b n)
  		| SIGMA (a, b, c) ->
		  	if (calx a 0) != (calx b 0) then
		  		if (calx a 0) < (calx b 0) then
  					(calx (SIGMA(INT((calx a 0) + 1), b, c)) (calx a n)) + (calx c (calx a n))
  				else
  					(calx (SIGMA(INT((calx a 0) - 1), b, c)) (calx a n)) + (calx c (calx a n))
  			else
				calx c (calx a n)
  		|_ ->
			n
  		
  let calculator : exp -> int
  = fun exp ->
  	calx exp 0