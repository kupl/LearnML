type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (cr1,cr2) = 
	match (cr1,cr2) with
	|(NIL,NIL) -> NIL
	|(NIL,ZERO c) -> ZERO c
	|(ZERO c, NIL) -> ZERO c
	|(NIL, ONE c) -> ONE c
	|(ONE c, NIL) -> ONE c
	|(NIL, MONE c) -> MONE c
	|(MONE c, NIL) -> MONE c
	|(ZERO c1, ZERO c2) -> ZERO(crazy2add(c1,c2))
	|(ZERO c1, ONE c2) -> ONE(crazy2add(c1,c2))
	|(ONE c1, ZERO c2) -> ONE(crazy2add(c1,c2))
	|(ZERO c1, MONE c2) -> MONE(crazy2add(c1,c2))
	|(MONE c1, ZERO c2) -> MONE(crazy2add(c1,c2))
	|(ONE c1, ONE c2) -> ZERO(crazy2add((ONE NIL),crazy2add(c1,c2)))
	|(ONE c1, MONE c2) -> ZERO(crazy2add(c1,c2))
	|(MONE c1, ONE c2) -> ZERO(crazy2add(c1,c2))
	|(MONE c1, MONE c2) -> ZERO(crazy2add((MONE NIL),crazy2add(c1,c2)))
