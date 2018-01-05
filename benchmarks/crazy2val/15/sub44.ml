type crazy2= NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2


let rec crazy2val (vallist:crazy2):int =
	match vallist with
	| NIL -> 0
	| ZERO i -> 2*(crazy2val i) 		 
	| ONE i -> 1 + 2*(crazy2val i)
	| MONE i -> -1 + 2*(crazy2val i)

