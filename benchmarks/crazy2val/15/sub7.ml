type crazy2 = 	NIL
		| ZERO of crazy2
		| ONE of crazy2
		| MONE of crazy2

let rec crazy2val (x: crazy2): int =
	match x with
	| NIL -> 0
	| ZERO NIL -> 0
	| ONE NIL -> 0
	| MONE NIL -> -1
	| ZERO x_post -> 0 + (2 * (crazy2val x_post))
	| ONE x_post -> 1 + (2 * (crazy2val x_post))
	| MONE x_post -> -1 + (2 * (crazy2val x_post))
