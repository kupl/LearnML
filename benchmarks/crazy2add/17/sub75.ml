type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (c1: crazy2) (c2: crazy2): crazy2 =
	match (c1, c2) with
	| (NIL, _) -> c2
	| (_, NIL) -> c1
	| (ZERO c3, ZERO c4) -> ZERO (crazy2add c3 c4)
	| (ZERO c3, ONE c4)  | (ONE c3, ZERO c4)  -> ONE (crazy2add c3 c4)
	| (ZERO c3, MONE c4) | (MONE c3, ZERO c4) -> MONE (crazy2add c3 c4)
	| (ONE c3, MONE c4)  | (MONE c3, ONE c4)  -> ZERO (crazy2add c3 c4)
	| (ONE c3, ONE c4)   -> ZERO (crazy2add ((crazy2add (ONE NIL) c3)) c4)
	| (MONE c3, MONE c4) -> ZERO (crazy2add ((crazy2add (MONE NIL) c3)) c4)

