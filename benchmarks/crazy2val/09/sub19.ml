type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val n = 
	match n with
		(ZERO a) -> 2*(crazy2val a)
		|(ONE a) -> 1+2*(crazy2val a)
		|(MONE a) -> 2*(crazy2val a)-1
		|NIL -> 0

