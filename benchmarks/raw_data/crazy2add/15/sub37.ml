type crazy2 = 
	| NIL 
	| ZERO of crazy2 
	| ONE of crazy2 
	| MONE of crazy2

(* crazy2add: crazy2 * crazy2 -> crazy2 *)
let rec crazy2add (c1, c2) =
	match (c1, c2) with
	| (NIL, _) -> c2
	| (_, NIL) -> c1
	| (ZERO ic1, ZERO ic2) -> (ZERO (crazy2add(ic1, ic2)))
	| (ZERO ic1, ONE ic2) -> (ONE (crazy2add(ic1, ic2)))
	| (ZERO ic1, MONE ic2) -> (MONE (crazy2add(ic1, ic2)))
	| (ONE ic1, ZERO ic2) -> (ONE (crazy2add(ic1, ic2)))
	| (ONE ic1, ONE ic2) -> (ZERO (crazy2add(crazy2add(ONE NIL, ic1), ic2)))
	| (ONE ic1, MONE ic2) -> (ZERO (crazy2add(ic1, ic2)))
	| (MONE ic1, ZERO ic2) -> (MONE (crazy2add(ic1, ic2)))
	| (MONE ic1, ONE ic2) -> (ZERO (crazy2add(ic1, ic2)))
	| (MONE ic1, MONE ic2) -> (ZERO (crazy2add(crazy2add(MONE NIL, ic1), ic2)))
