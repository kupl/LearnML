type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (c1, c2) =
	match (c1, c2) with
	| (NIL, v) -> v
	| (v, NIL) -> v
	| (ZERO v1, ZERO v2) -> ZERO (crazy2add (v1, v2))
	| (ZERO v1, ONE v2) -> ONE (crazy2add (v1, v2))
	| (ZERO v1, MONE v2) -> MONE (crazy2add (v1, v2))
	| (ONE v1, ZERO v2) -> ONE (crazy2add (v1, v2))
	| (ONE v1, ONE v2) -> ZERO (crazy2add (ONE(NIL), (crazy2add(v1, v2))))
	| (ONE v1, MONE v2) -> ZERO (crazy2add (v1, v2))
	| (MONE v1, ZERO v2) -> MONE (crazy2add (v1, v2))
	| (MONE v1, ONE v2) -> ZERO (crazy2add (v1, v2))
	| (MONE v1, MONE v2) -> ZERO (crazy2add (MONE(NIL), (crazy2add(v1, v2))))

