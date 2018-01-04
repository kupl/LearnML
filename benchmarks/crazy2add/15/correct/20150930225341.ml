(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #2 *)
type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2

let rec crazy2add (p : crazy2 * crazy2) : crazy2 =
	match p with
	| (a, NIL) -> a
	| (NIL, b) -> b
	| (ZERO x, ZERO y) -> ZERO (crazy2add(x, y))
	| (ZERO x, ONE y) -> ONE (crazy2add(x, y))
	| (ZERO x, MONE y) -> MONE (crazy2add(x, y))
	| (ONE x, ZERO y) -> ONE (crazy2add(x, y))
	| (ONE x, ONE y) -> ZERO (crazy2add(crazy2add(x, y), ONE NIL))
	| (ONE x, MONE y) -> ZERO (crazy2add(x, y))
	| (MONE x, ZERO y) -> MONE (crazy2add(x, y))
	| (MONE x, ONE y) -> ZERO (crazy2add(x, y))
	| (MONE x, MONE y) -> ZERO (crazy2add(crazy2add(x, y), MONE NIL))
