type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun cry2 ->
	match cry2 with
	| NIL -> 0
	| ZERO cry -> 0 + 2*crazy2val(cry)
	| ONE cry -> 1 + 2*crazy2val(cry)
	| MONE cry -> -1 + 2*crazy2val(cry)

