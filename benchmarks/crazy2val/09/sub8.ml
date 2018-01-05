exception Error of string;;
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;
let crazy2val ( v : crazy2 ) =
	let rec crazy2valrec ( v : crazy2 ) =
		match v with
			NIL -> 0 |
			ZERO p -> 2 * crazy2valrec( p ) |
			ONE p -> 2 * crazy2valrec( p ) + 1 |
			MONE p -> 2 * crazy2valrec( p ) - 1 in
	match v with
		NIL -> raise ( Error "Should not be NIL" ) |
		_ -> crazy2valrec( v );;
