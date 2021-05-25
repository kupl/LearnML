type crazy2 =	NIL
		| ZERO of crazy2
		| ONE of crazy2
		| MONE of crazy2

let rec crazy2add ((l:crazy2),(r:crazy2)) : crazy2 =
	match l, r with
	| NIL, _ 		-> r
	| _, NIL 		-> l
        | ZERO l_, ZERO r_ 	-> ZERO(crazy2add(l_, r_))
	| ZERO l_, MONE r_ 	-> MONE(crazy2add(l_, r_))
	| ZERO l_, ONE r_ 	-> ONE(crazy2add(l_, r_))
	| MONE l_, ZERO r_ 	-> MONE(crazy2add(l_, r_))
	| ONE l_, ZERO r_	-> ONE(crazy2add(l_, r_))
	| ONE l_, MONE r_	-> ZERO(crazy2add(l_, r_))
	| MONE l_, ONE r_	-> ZERO(crazy2add(l_, r_))
	| ONE l_, ONE r_	-> ZERO(crazy2add(ONE NIL, crazy2add(l_, r_)))
	| MONE l_, MONE r_	-> ZERO(crazy2add(MONE NIL, crazy2add(l_, r_)))

