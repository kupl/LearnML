type crazy2 = NIL 
			| ZERO of crazy2 
			| ONE of crazy2 
			| MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (c1,c2) ->
	match (c1,c2) with
	|(NIL, NIL) -> NIL
	|(NIL, b) -> b
	|(a, NIL) -> a
	|(ZERO a, ZERO b) -> ZERO (crazy2add (a,b))
	|(ZERO a, ONE b) -> ONE (crazy2add (a,b))
	|(ZERO a, MONE b) -> MONE (crazy2add (a,b))
	|(ONE a, ZERO b) -> ONE (crazy2add (a,b))
	|(ONE a, ONE b) -> ZERO (crazy2add (ONE NIL, (crazy2add (a,b))))
	|(ONE a, MONE b) -> ZERO (crazy2add (a,b))
	|(MONE a, ZERO b) -> MONE (crazy2add (a,b))
	|(MONE a, ONE b) -> ZERO (crazy2add (a,b))
	|(MONE a, MONE b) -> ZERO (crazy2add (MONE NIL, (crazy2add (a,b))))


