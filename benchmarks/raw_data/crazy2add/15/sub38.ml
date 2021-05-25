type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add ((c1: crazy2), (c2: crazy2)): crazy2 =
	match (c1,c2) with 
	| (NIL, NIL) -> NIL
	| (NIL, ZERO c) -> ZERO c
	| (NIL, ONE c) -> ONE c
	| (NIL, MONE c) -> MONE c
	| (ZERO c, NIL) -> ZERO c
	| (ZERO c, ZERO r) -> ZERO(crazy2add(c,r))
	| (ZERO c, ONE r) -> ONE(crazy2add(c,r))
	| (ZERO c, MONE r) -> MONE(crazy2add(c,r))
	| (ONE c, NIL) -> ONE c
	| (ONE c, ZERO r) -> ONE(crazy2add(c,r))
	| (ONE c, ONE r) -> ZERO(crazy2add ((ONE NIL),(crazy2add(c,r))))
	| (ONE c, MONE r) -> ZERO(crazy2add(c,r))
	| (MONE c, NIL) -> MONE c
	| (MONE c, ZERO r) -> MONE(crazy2add(c,r))
	| (MONE c, ONE r) -> ZERO(crazy2add(c,r))
	| (MONE c, MONE r) -> ZERO(crazy2add((MONE NIL),(crazy2add(c,r))))