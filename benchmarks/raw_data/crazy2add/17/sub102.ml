type crazy2 = NIL
						| ZERO of crazy2
						| ONE of crazy2
						| MONE of crazy2

let addOne cra carry =
	let rec add cra carry =
		match cra with
		| NIL ->
		(	if carry == 0 then NIL
			else if carry == 1 then ONE NIL
			else MONE NIL )
		| ONE a ->
		( if carry == 0 then ONE a
			else if carry == 1 then ZERO(add a 1)
			else ZERO a )
		| ZERO a ->
		( if carry == 0 then ZERO a
			else if carry == 1 then ONE a
			else MONE a )
		| MONE a ->
		( if carry == 0 then MONE a
			else if carry == 1 then ZERO a
			else ZERO(add a 2) ) in
	add cra carry

let crazy2add (cra1, cra2) =
	let rec cradd c1 c2 carry =
		match (c1, c2) with
		| (NIL, a) -> addOne c2 carry
		| (a, NIL) -> addOne c1 carry
		|	(ONE a, ONE b) ->
			(	if carry == 0 then ZERO(cradd a b 1)
				else if carry == 1 then ONE(cradd a b 1)
				else ONE(cradd a b 0) )
		| (ONE a, ZERO b) ->
			(	if carry == 1 then ZERO(cradd a b 1)
				else if carry == 0 then ONE(cradd a b 0)
				else ZERO(cradd a b 0) )
		| (ZERO a, ONE b) ->
			(	if carry == 1 then ZERO(cradd a b 1)
				else if carry == 0 then ONE(cradd a b 0)
				else ZERO(cradd a b 0) )
		| (ONE a, MONE b)->
			(	if carry == 1 then ONE(cradd a b 0)
				else if carry == 0 then ZERO (cradd a b 0)
				else MONE(cradd a b 0) )
		| (MONE a, ONE b) ->
			(	if carry == 1 then ONE(cradd a b 0)
				else if carry == 0 then ZERO (cradd a b 0)
				else MONE(cradd a b 0) )		
		| (ZERO a, ZERO b) ->
			( if carry == 1 then ONE(cradd a b 0)
				else if carry == 0 then ZERO(cradd a b 0)
				else MONE(cradd a b 0))
		| (ZERO a, MONE b) ->
			(	if carry == 1 then ZERO (cradd a b 0)
				else if carry == 0 then MONE(cradd a b 0)
				else ZERO(cradd a b 2) )
		| (MONE a, ZERO b) ->
			(	if carry == 1 then ZERO (cradd a b 0)
				else if carry == 0 then MONE(cradd a b 0)
				else ZERO(cradd a b 2) )
		| (MONE a, MONE b) ->
			( if carry == 1 then MONE(cradd a b 0)
				else if carry == 0 then ZERO(cradd a b 2)
				else MONE(cradd a b 2)) in
		cradd cra1 cra2 0

