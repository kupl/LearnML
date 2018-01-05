type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (c:crazy2) =
	match c with
		| NIL -> 0
		| ONE(c) -> 1 + 2 * (crazy2val c)
  		| MONE(c) -> -1 + 2 * (crazy2val c)
  		| ZERO(c) -> 2 * (crazy2val c)
