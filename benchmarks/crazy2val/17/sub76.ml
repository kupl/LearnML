type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (c : crazy2) : int =
	match c with
	|NIL -> 0
	|ZERO(c0) -> 0 + 2*crazy2val(c0)
	|ONE(c0) -> 1 + 2*crazy2val(c0)
	|MONE(c0) -> -1 + 2*crazy2val(c0)