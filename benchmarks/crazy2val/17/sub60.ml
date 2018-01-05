(*Lee SEok Jin 2013-11417 CSE HW2_2*)

type crazy2	= NIL
		| ZERO of crazy2
		| ONE of crazy2 
		| MONE of crazy2

let rec crazy2val(c: crazy2): int = 
	match c with
	| NIL -> 0
	| ZERO(n) -> 2*crazy2val(n)
	| ONE(n) -> 1 + 2*crazy2val(n)
	| MONE(n) -> 2*crazy2val(n)-1 
