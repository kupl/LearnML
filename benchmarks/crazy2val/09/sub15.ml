type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val n =
	match n with
		NIL -> 0
	| ZERO m ->  0 + 2 * (crazy2val m)
	| ONE  m ->  1 + 2 * (crazy2val m)
	| MONE m -> -1 + 2 * (crazy2val m)