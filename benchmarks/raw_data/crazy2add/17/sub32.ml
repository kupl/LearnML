type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (c1, c2) ->
	match (c1, c2) with
	| (NIL, _) -> c2
	| (_, NIL) -> c1
	| (ZERO x, ZERO y) -> ZERO (crazy2add (x, y))
	| (ZERO x, ONE y) -> ONE (crazy2add (x, y))
	| (ZERO x, MONE y) -> MONE (crazy2add (x, y))
	| (ONE x, ZERO y) -> ONE (crazy2add (x, y))
	| (ONE x, ONE y) -> ZERO (crazy2add (ONE NIL, crazy2add (x, y)))
	| (ONE x, MONE y) -> ZERO (crazy2add (x, y))
	| (MONE x, ZERO y) -> MONE (crazy2add (x, y))
	| (MONE x, ONE y) -> ZERO (crazy2add (x, y))
	| (MONE x, MONE y) -> ZERO (crazy2add (MONE NIL, crazy2add (x, y)))


