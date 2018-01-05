
type crazy2 = NIL
	    | ZERO of crazy2
	    | ONE of crazy2
	    | MONE of crazy2


let rec crazy2val crazy2 = match crazy2 with
			 | NIL -> 0
			 | ZERO NIL -> 0
			 | ONE NIL -> 1
			 | MONE NIL -> -1
			 | ZERO(c2) -> 2 * crazy2val c2
			 | ONE(c2) -> 1 + 2 * crazy2val c2
			 | MONE(c2) -> -1 + 2 * crazy2val c2
