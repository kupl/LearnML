type crazy2 = 	NIL
		| ZERO of crazy2
		| ONE of crazy2
		| MONE of crazy2


let rec crazy2add ((c1: crazy2), (c2: crazy2)) : crazy2 =
	match (c1, c2) with
	| (NIL, _) -> c2
	| (_, NIL) -> c1
	| (ZERO c1s, ONE c2s) -> ONE (crazy2add (c1s, c2s))
	| (ZERO c1s, MONE c2s) -> MONE (crazy2add (c1s, c2s))
	| (ONE c1s, ZERO c2s) -> ONE (crazy2add (c1s, c2s))
	| (ZERO c1s, ZERO c2s) -> ZERO (crazy2add (c1s, c2s))
	| (MONE c1s, ZERO c2s) -> MONE (crazy2add (c1s, c2s))
	| (ONE c1s, MONE c2s) -> ZERO (crazy2add (c1s, c2s))
	| (ONE c1s, ONE c2s) -> ZERO (crazy2add(ONE NIL, crazy2add (c1s, c2s)))
	| (MONE c1s, MONE c2s) -> ZERO (crazy2add (MONE NIL, crazy2add (c1s, c2s)))
	| (MONE c1s, ONE c2s) -> ZERO (crazy2add (c1s, c2s))
