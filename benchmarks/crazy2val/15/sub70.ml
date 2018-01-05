type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val x =
	match x with
	| NIL -> 0
	| ZERO(y) -> 2*crazy2val(y) 
	| ONE(y) -> 2*crazy2val(y) + 1
	| MONE(y) -> 2*crazy2val(y) -1

