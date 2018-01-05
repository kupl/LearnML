(*Lee Seok JIn 2013-11417 CSE hw2_3*)

type crazy2	= NIL
		| ZERO of crazy2
		| ONE of crazy2 
		| MONE of crazy2

let rec crazy2add((cl,cr): crazy2*crazy2): crazy2 =
	match (cl, cr) with
	| (_, NIL) -> cl
	| (NIL, _) -> cr
	| (ZERO(l), ZERO(r)) -> ZERO(crazy2add(l,r))
	| (ZERO(l), ONE(r)) -> ONE(crazy2add(l,r))
	| (ZERO(l), MONE(r)) -> MONE(crazy2add(l,r))
	| (ONE(l), ZERO(r)) -> ONE(crazy2add(l,r))
	| (ONE(l), ONE(r)) -> ZERO(crazy2add(crazy2add(l,r), ONE NIL))
	| (ONE(l), MONE(r)) -> ZERO(crazy2add(l,r))
	| (MONE(l), ZERO(r)) -> MONE(crazy2add(l,r))
	| (MONE(l), ONE(r)) -> ZERO(crazy2add(l,r))
	| (MONE(l), MONE(r)) -> ZERO(crazy2add(crazy2add(l,r), MONE NIL))
