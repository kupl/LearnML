type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add a b =
	let rec crazy2val n =
		match n with
			NIL -> 0
		| ZERO m ->  0 + 2 * (crazy2val m)
		| ONE  m ->  1 + 2 * (crazy2val m)
		| MONE m -> -1 + 2 * (crazy2val m)
	in
	let rec tocrazy2 n =
		if n=0
			then NIL
		else if n mod 2 = 0
			then ZERO (tocrazy2 (n/2))
		else if n > 0
			then ONE  (tocrazy2 (n/2))
		else
					 MONE (tocrazy2 (n/2))
	in
	tocrazy2 ((crazy2val a)+(crazy2val b))