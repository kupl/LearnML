type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

let rec crazy2add a b =
	
	match a, b with
	| NIL, NIL -> NIL
	| a, NIL -> a
	| NIL, b -> b
	| ZERO a, ZERO b -> (ZERO (crazy2add a b))
	| ONE a, ZERO b -> (ONE (crazy2add a b))
	| ZERO a, ONE b -> (ONE (crazy2add a b))
	| ZERO a, MONE b -> (MONE (crazy2add a b))
	| MONE a, ZERO b -> (MONE (crazy2add a b))
	| ONE a, MONE b -> (ZERO (crazy2add a b))
	| MONE a, ONE b -> (ZERO (crazy2add a b))
	| ONE a, ONE b -> (ZERO (crazy2add (crazy2add (ONE NIL) a) b))
	| MONE a, MONE b -> (ZERO (crazy2add (crazy2add (MONE NIL) a) b))
;;