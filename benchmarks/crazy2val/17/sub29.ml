type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val crazy = match crazy with
	| NIL -> 0
	| ZERO next -> (crazy2val next) * 2
	| ONE next -> (crazy2val next) * 2 + 1
	| MONE next -> (crazy2val next) * 2 - 1