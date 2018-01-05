type crazy2 = NIL |ZERO of crazy2 |ONE of crazy2 |MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (c1, c2) ->
	match c1 with
	| NIL ->
		(match c2 with
		| NIL -> NIL
		| ZERO cc2 -> ZERO(crazy2add (NIL, cc2))
		| ONE cc2 -> ONE(crazy2add (NIL, cc2))
		| MONE cc2 -> MONE(crazy2add (NIL, cc2))
		)
	| ZERO cc1 ->
		(match c2 with
		| NIL -> ZERO(crazy2add (cc1, NIL))
		| ZERO cc2 -> ZERO(crazy2add (cc1, cc2))
		| ONE cc2 -> ONE(crazy2add (cc1, cc2))
		| MONE cc2 -> MONE(crazy2add (cc1, cc2))
		)
	| ONE cc1 ->
		(match c2 with
		| NIL -> ONE(crazy2add (cc1, NIL))
		| ZERO cc2 -> ONE(crazy2add (cc1, cc2)) 
		| ONE cc2 -> ZERO(crazy2add ((ONE NIL), crazy2add (cc1, cc2)))
		| MONE cc2 -> ZERO(crazy2add (cc1, cc2))
		)
	| MONE cc1 ->
		(match c2 with
		| NIL -> MONE(crazy2add (cc1, NIL))
		| ZERO cc2 -> MONE(crazy2add (cc1, cc2))
		| ONE cc2 -> ZERO(crazy2add (cc1, cc2))
		| MONE cc2 -> ZERO(crazy2add ((MONE NIL), crazy2add (cc1, cc2)))
		)