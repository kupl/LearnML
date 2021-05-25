type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (add1, add2) =
	match add1, add2 with
	|(l1, NIL) -> l1
	|(NIL, l2) -> l2
	|(ZERO l1, ZERO l2) -> ZERO( crazy2add (l1, l2))
	|(ONE l1, ZERO l2) -> ONE( crazy2add (l1, l2))
	|(ZERO l1, ONE l2) -> ONE( crazy2add (l1, l2))
	|(ZERO l1, MONE l2) -> MONE( crazy2add (l1, l2))
	|(MONE l1, ZERO l2) -> MONE( crazy2add (l1, l2))
	|(MONE l1, ONE l2) -> ZERO( crazy2add (l1, l2))
	|(ONE l1, MONE l2) -> ZERO( crazy2add (l1, l2))
	|(ONE l1, ONE l2) -> crazy2add( ONE NIL, crazy2add (l1, l2))
	|(MONE l1, MONE l2) -> crazy2add( MONE NIL, crazy2add (l1, l2))
	
