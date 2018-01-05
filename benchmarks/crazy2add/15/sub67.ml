type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val x =
	match x with
	| NIL -> 0
	| ZERO(y) -> 2*crazy2val(y) 
	| ONE(y) -> 2*crazy2val(y) + 1
	| MONE(y) -> 2*crazy2val(y) -1

let rec crazy2add (x,y) = 
	match x,y with
	| x, NIL -> x
	| NIL, y -> y
	| MONE(x'), ONE(y') | ZERO(x'), ZERO(y') | ONE(x'), MONE(y') -> ZERO(crazy2add(x',y'))
	| MONE(x'), ZERO(y') | ZERO(x'), MONE(y') -> MONE(crazy2add(x',y'))
	| ONE(x'), ZERO(y') | ZERO(x'), ONE(y') ->  ONE(crazy2add(x',y'))
	| MONE(x'), MONE(y') -> ZERO(crazy2add(crazy2add(x',y'),MONE(NIL)))
	| ONE(x'), ONE(y') -> ZERO(crazy2add(crazy2add(x',y'),ONE(NIL)))