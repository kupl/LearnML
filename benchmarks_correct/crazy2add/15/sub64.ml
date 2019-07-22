type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

let rec crazy2add (c1, c2) =
	match c1 with
	| NIL -> c2
	| ZERO a -> (match c2 with
				| NIL -> c1
				| ZERO b -> ZERO(crazy2add(a, b))
				| ONE b -> ONE(crazy2add(a, b))
				| MONE b -> MONE(crazy2add(a, b)))
	| ONE a -> (match c2 with
				| NIL -> c1
				| ZERO b -> ONE(crazy2add(a, b))
				| ONE b -> ZERO(crazy2add(ONE NIL, crazy2add(a, b)))
				| MONE b -> ZERO(crazy2add(a, b)))
	| MONE a -> (match c2 with
				| NIL -> c1
				| ZERO b -> MONE(crazy2add(a, b))
				| ONE b -> ZERO(crazy2add(a, b))
				| MONE b -> ZERO(crazy2add(MONE NIL, crazy2add(a, b))));;
