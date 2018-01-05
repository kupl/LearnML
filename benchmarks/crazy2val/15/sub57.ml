type crazy2 = NIL 
			| ZERO of crazy2 
			| ONE of crazy2 
			| MONE of crazy2

let rec crazy2val: crazy2 -> int =
	fun c2 -> match c2 with
			| NIL -> 0
			| ZERO c -> 2*(crazy2val c)
			| ONE c -> 2*(crazy2val c) + 1
			| MONE c -> 2*(crazy2val c) - 1
