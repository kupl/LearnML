type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val num = 
	match num with
	| NIL -> 0
	| ZERO num' -> (crazy2val num') * 2
	| ONE num' ->(crazy2val num') * 2 + 1
	| MONE num' -> (crazy2val num') * 2 - 1	
