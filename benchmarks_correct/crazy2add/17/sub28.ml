type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add(a, b) = match (a, b) with
	| (ONE c, ONE d) -> ZERO(crazy2add(crazy2add(ONE NIL, c), d))
	| (MONE c, MONE d) -> ZERO(crazy2add(crazy2add(MONE NIL, c), d))
	| (ZERO c, ZERO d) | (ONE c, MONE d) -> ZERO(crazy2add(c, d))
	| (ZERO c, ONE d) -> ONE(crazy2add(c, d))
	| (ZERO c, MONE d) -> MONE(crazy2add(c, d))
	| (NIL, b) -> b
	| (_, _) -> crazy2add(b, a)