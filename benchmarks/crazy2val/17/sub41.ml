type crazy2 =	NIL
		| ZERO of crazy2
		| ONE of crazy2
		| MONE of crazy2

let rec crazy2val (number : crazy2) : int =
	match number with
	| NIL ->	0
	| ZERO x ->	2 * (crazy2val x)
	| ONE x ->	2 * (crazy2val x) + 1
	| MONE x ->	2 * (crazy2val x) - 1

