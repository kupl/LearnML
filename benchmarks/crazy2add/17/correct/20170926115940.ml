type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2add (c : crazy2 * crazy2) = 
	let rec sub (c : crazy2 * crazy2) (carry : crazy2) =
		match carry with
		| ZERO NIL -> 
			(match c with
			| NIL, NIL -> NIL
			| ZERO c1, NIL -> ZERO (sub (c1, NIL) (ZERO (NIL)))
			| ONE c1, NIL -> ONE (sub (c1, NIL) (ZERO (NIL)))
			| MONE c1, NIL -> MONE (sub (c1, NIL) (ZERO (NIL)))
			| NIL, ZERO c2 -> ZERO (sub (NIL, c2) (ZERO (NIL)))
			| NIL, ONE c2 -> ONE (sub (NIL, c2) (ZERO (NIL)))
			| NIL, MONE c2 -> MONE (sub (NIL, c2) (ZERO (NIL)))
			| ZERO c1, ZERO c2 -> ZERO (sub (c1, c2) (ZERO (NIL)))
			| ZERO c1, ONE c2 -> ONE (sub (c1, c2) (ZERO (NIL)))
			| ZERO c1, MONE c2 -> MONE (sub (c1, c2) (ZERO (NIL)))
			| ONE c1, ZERO c2 -> ONE (sub (c1, c2) (ZERO (NIL)))
			| ONE c1, ONE c2 -> ZERO (sub (c1, c2) (ONE (NIL)))
			| ONE c1, MONE c2 -> ZERO (sub (c1, c2) (ZERO (NIL)))
			| MONE c1, ZERO c2 -> MONE (sub (c1, c2) (ZERO (NIL)))
			| MONE c1, ONE c2 -> ZERO (sub (c1, c2) (ZERO (NIL)))
			| MONE c1, MONE c2 -> ZERO (sub (c1, c2) (MONE (NIL))))
		| ONE NIL ->
			(match c with
			| NIL, NIL -> ONE (NIL)
			| ZERO c1, NIL -> ONE (sub (c1, NIL) (ZERO (NIL)))
			| ONE c1, NIL -> ZERO (sub (c1, NIL) (ONE (NIL)))
			| MONE c1, NIL -> ZERO (sub (c1, NIL) (ZERO (NIL)))
			| NIL, ZERO c2 -> ONE (sub (NIL, c2) (ZERO (NIL)))
			| NIL, ONE c2 -> ZERO (sub (NIL, c2) (ONE (NIL)))
			| NIL, MONE c2 -> ZERO (sub (NIL, c2) (ZERO (NIL)))
			| ZERO c1, ZERO c2 -> ONE (sub (c1, c2) (ZERO (NIL)))
			| ZERO c1, ONE c2 -> ZERO (sub (c1, c2) (ONE (NIL)))
			| ZERO c1, MONE c2 -> ZERO (sub (c1, c2) (ZERO (NIL)))
			| ONE c1, ZERO c2 -> ZERO (sub (c1, c2) (ONE (NIL)))
			| ONE c1, ONE c2 -> ONE (sub (c1, c2) (ONE (NIL)))
			| ONE c1, MONE c2 -> ONE (sub (c1, c2) (ZERO (NIL)))
			| MONE c1, ZERO c2 -> ZERO (sub (c1, c2) (ZERO (NIL)))
			| MONE c1, ONE c2 -> ONE (sub (c1, c2) (ZERO (NIL)))
			| MONE c1, MONE c2 -> MONE (sub (c1, c2) (ZERO (NIL))))
		| MONE (NIL) ->
			(match c with
			| NIL, NIL -> MONE (NIL)
			| ZERO c1, NIL -> MONE (sub (c1, NIL) (ZERO (NIL)))
			| ONE c1, NIL -> ZERO (sub (c1, NIL) (ZERO (NIL)))
			| MONE c1, NIL -> ZERO (sub (c1, NIL) (MONE (NIL)))
			| NIL, ZERO c2 -> MONE (sub (NIL, c2) (ZERO (NIL)))
			| NIL, ONE c2 -> ZERO (sub (NIL, c2) (ZERO (NIL)))
			| NIL, MONE c2 -> ZERO (sub (NIL, c2) (MONE (NIL)))
			| ZERO c1, ZERO c2 -> MONE (sub (c1, c2) (ZERO (NIL)))
			| ZERO c1, ONE c2 -> ZERO (sub (c1, c2) (ZERO (NIL)))
			| ZERO c1, MONE c2 -> ZERO (sub (c1, c2) (MONE (NIL)))
			| ONE c1, ZERO c2 -> ZERO (sub (c1, c2) (ZERO (NIL)))
			| ONE c1, ONE c2 -> ONE (sub (c1, c2) (ZERO (NIL)))
			| ONE c1, MONE c2 -> MONE (sub (c1, c2) (ZERO (NIL)))
			| MONE c1, ZERO c2 -> ZERO (sub (c1, c2) (MONE (NIL)))
			| MONE c1, ONE c2 -> MONE (sub (c1, c2) (ZERO (NIL)))
			| MONE c1, MONE c2 -> MONE (sub (c1, c2) (MONE (NIL))))
		| _ -> NIL (* not used *)
	in sub c (ZERO (NIL))
