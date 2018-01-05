type crazy2 = 	NIL
		| ZERO of crazy2
		| ONE of crazy2
		| MONE of crazy2


let rec crazy2val (c: crazy2) : int =
	match c with
	| NIL -> 0
	| ZERO cs -> 0 + 2 * crazy2val(cs) 
	| ONE cs -> 1 + 2 * crazy2val(cs)
	| MONE cs -> -1 + 2 * crazy2val(cs)
		
