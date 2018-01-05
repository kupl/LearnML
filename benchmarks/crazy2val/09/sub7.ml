type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;
let rec crazy2val ( v : crazy2 ) =
	match v with
		NIL -> 0 |
		ZERO p -> 2 * crazy2val( p ) |
		ONE p -> 2 * crazy2val( p ) + 1 |
		MONE p -> 2 * crazy2val( p ) - 1;;