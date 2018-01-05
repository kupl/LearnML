type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val c =
	match c with
	  NIL -> 0
	| ZERO v -> 0 + 2 * (crazy2val v)
	| ONE v -> 1 + 2 * (crazy2val v)
	| MONE v -> -1 + 2 * (crazy2val v)

