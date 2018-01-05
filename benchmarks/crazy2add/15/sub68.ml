let rec crazy2add (x,y) = 
	match x,y with
	| n, NIL | NIL, n -> n
	| MONE(x'), ONE(y') | ZERO(x'), ZERO(y') | ONE(x'), MONE(y') -> ZERO(crazy2add(x',y'))
	| MONE(x'), ZERO(y') | ZERO(x'), MONE(y') -> MONE(crazy2add(x',y'))
	| ONE(x'), ZERO(y') | ZERO(x'), ONE(y') ->  ONE(crazy2add(x',y'))
	| MONE(x'), MONE(y') -> ZERO(crazy2add(crazy2add(x',y'),MONE(NIL)))
	| ONE(x'), ONE(y') -> ZERO(crazy2add(crazy2add(x',y'),ONE(NIL)))