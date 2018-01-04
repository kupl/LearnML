let rec crazy2add (c1, c2) =
	match(c1, c2) with
	| (NIL, _) -> c2
	| (_, NIL) -> c1
	| (ZERO l1, MONE l2) -> MONE (crazy2add (l1, l2))
	| (ZERO l1, ZERO l2) -> ZERO (crazy2add (l1, l2))
	| (ZERO l1, ONE l2) -> ONE (crazy2add (l1, l2))
	| (ONE l1, MONE l2) -> ZERO (crazy2add (l1, l2))
	| (ONE l1, ZERO l2) -> ONE (crazy2add (l1, l2))
	| (ONE l1, ONE l2) -> ZERO (crazy2add ((crazy2add (l1, l2)), (ONE NIL)))
	| (MONE l1, MONE l2) -> ZERO (crazy2add ((crazy2add (l1, l2)), (MONE NIL)))
	| (MONE l1, ZERO l2) -> MONE (crazy2add (l1, l2))	| (MONE l1, ONE l2) -> ZERO (crazy2add (l1, l2))

