
type crazy2 = NIL
	    | ZERO of crazy2
	    | ONE of crazy2
	    | MONE of crazy2

let rec crazy2add (c1,c2) = match (c1,c2) with
			  | (NIL,c2) -> c2
			  | (c1,NIL) -> c1
			  | (ZERO tc1, ZERO tc2) -> ZERO(crazy2add(tc1,tc2))
			  | (ZERO tc1, ONE tc2) -> ONE(crazy2add(tc1,tc2))
			  | (ZERO tc1, MONE tc2) -> MONE(crazy2add(tc1,tc2))
			  | (ONE tc1, ZERO tc2) -> ONE(crazy2add(tc1,tc2))
			  | (ONE tc1, ONE tc2) -> ZERO(crazy2add(ONE NIL,crazy2add(tc1,tc2)))
			  | (ONE tc1, MONE tc2) -> ZERO(crazy2add(tc1,tc2))
			  | (MONE tc1, ZERO tc2) -> MONE(crazy2add(tc1,tc2))
			  | (MONE tc1, ONE tc2) -> ZERO(crazy2add(tc1,tc2))
			  | (MONE tc1, MONE tc2) -> ZERO(crazy2add(MONE NIL,crazy2add(tc1,tc2)))
