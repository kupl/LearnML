(* Department: Electrical and Computer Engineering *)
(* Student ID: 2010-11834 *)
(* Name: Kwonjoon Lee *)
(* Exercise #1 *)
type crazy2 = NIL
			| ZERO of crazy2
			| ONE of crazy2
			| MONE of crazy2

let rec crazy2val (f : crazy2) : int =
	match f with
	| NIL -> 0
	| ZERO x -> 2 * crazy2val(x)
	| ONE x -> 1 + 2 * crazy2val(x)
	| MONE x -> -1 + 2 * crazy2val(x)
