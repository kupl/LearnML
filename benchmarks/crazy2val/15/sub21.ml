type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2 
			| MONE of crazy2

let rec crazy2val c = 
					match c with
					NIL			-> 0
					|ZERO(d)	-> 0 + 2 * crazy2val d
					|ONE(d)		-> 1 + 2 * crazy2val d
					|MONE(d)	-> (-1) + 2 * crazy2val d
