 type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add ((a : crazy2), (b : crazy2)) : crazy2 =
		match (a, b) with
		| (ONE m, ONE n) -> (ZERO (crazy2add ((ONE NIL), (crazy2add (m, n)))))
		| (ONE m, ZERO n) | (ZERO m, ONE n) -> (ONE (crazy2add (m, n)))
		| (ONE m, MONE n) | (ZERO m, ZERO n) | (MONE m, ONE n) -> (ZERO (crazy2add (m, n)))
		| (MONE m, ZERO n) | (ZERO m, MONE n) -> (MONE (crazy2add (m, n)))
		| (MONE m, MONE n) -> (ZERO (crazy2add ((MONE NIL), (crazy2add (m, n)))))
		| (NIL, b) -> b
		| (a, NIL) -> a			   
