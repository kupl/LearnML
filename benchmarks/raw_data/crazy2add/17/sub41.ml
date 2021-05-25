type crazy2 = NIL
	| ZERO of crazy2
	| ONE of crazy2
	| MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun a ->
	match a with
	(NIL, NIL) -> NIL
	| (NIL, n) -> n
	| (n, NIL) -> n
	| (ZERO(n1), ZERO(n2)) -> ZERO (crazy2add (n1, n2))
	| (ZERO(n1), ONE(n2)) -> ONE (crazy2add (n1, n2))
	| (ZERO(n1), MONE(n2)) -> MONE (crazy2add (n1, n2))
	| (ONE(n1), ZERO(n2)) -> ONE (crazy2add (n1, n2))
	| (ONE(n1), ONE(n2)) -> ZERO (crazy2add (crazy2add (n1, n2), ONE(NIL)))
	| (ONE(n1), MONE(n2)) -> ZERO (crazy2add (n1, n2))
	| (MONE(n1), ZERO(n2)) -> MONE (crazy2add (n1, n2))
	| (MONE(n1), ONE(n2)) -> ZERO (crazy2add (n1, n2))
	| (MONE(n1), MONE(n2)) -> ZERO (crazy2add (crazy2add (n1, n2), MONE(NIL)))

