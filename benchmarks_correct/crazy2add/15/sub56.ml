type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (num1, num2) = 
	match num1 with
	| NIL -> num2
	| ZERO num1' -> 
	(match num2 with 
	 | NIL -> num1
	 | ZERO num2' -> ZERO(crazy2add (num1', num2'))
	 | ONE num2'  -> ONE (crazy2add (num1', num2'))
	 | MONE num2' -> MONE(crazy2add (num1', num2')) )
	| ONE num1' ->
	(match num2 with 
	 | NIL -> num1
	 | ZERO num2' -> ONE (crazy2add (num1', num2'))
	 | ONE num2'  -> ZERO(crazy2add (ONE (NIL), crazy2add (num1', num2')))
	 | MONE num2' -> ZERO(crazy2add (num1', num2')) )
	| MONE num1' ->
	(match num2 with
	 | NIL -> num1
	 | ZERO num2' -> MONE(crazy2add (num1', num2'))
	 | ONE num2'  -> ZERO(crazy2add (num1', num2'))
	 | MONE num2' -> ZERO(crazy2add (MONE (NIL), crazy2add (num1', num2'))) )
