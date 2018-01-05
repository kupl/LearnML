type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (n: crazy2): int =
	match n with
	| NIL 		-> 0
	| ZERO n1 	-> 2*(crazy2val n1)
	| ONE n1 	-> 1 + 2*(crazy2val n1)
	| MONE n1	-> -1 + 2*(crazy2val n1)

