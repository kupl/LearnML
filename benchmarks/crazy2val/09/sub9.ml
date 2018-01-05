(*2006-11720 Kim Eunsol HW1 #6*)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val(ab) =
	match ab with ZERO b -> 0 + 2*crazy2val(b)
	|ONE b -> 1 + 2*crazy2val(b)
	|MONE b -> -1 + 2*crazy2val(b)
	|NIL -> 0
