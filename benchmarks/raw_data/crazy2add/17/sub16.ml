type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (n1, n2) =
	let rec func (n1, n2, p) =
		match (n1, n2, p) with
		| (NIL, NIL, _) -> p
		| (NIL, _, NIL) -> n2
		| (NIL, ZERO c, ONE NIL) -> ONE c
		| (NIL, ZERO c, MONE NIL) -> MONE c
		| (NIL, ONE c, ONE NIL) -> ZERO (func (n1, c, ONE NIL))
		| (NIL, ONE c, MONE NIL) -> ZERO c
		| (NIL, MONE c, ONE NIL) -> ZERO c
		| (NIL, MONE c, MONE NIL) -> ZERO (func (n1, c, MONE NIL))
		| (_, NIL, NIL) -> n1
		| (ZERO c, NIL, ONE NIL) -> ONE c
		| (ZERO c, NIL, MONE NIL) -> MONE c
		| (ONE c, NIL, ONE NIL) -> ZERO (func (c, n2, ONE NIL))
		| (ONE c, NIL, MONE NIL) -> ZERO c
		| (MONE c, NIL, ONE NIL) -> ZERO c
		| (MONE c, NIL, MONE NIL) -> ZERO (func (c, n2, MONE NIL))
		| (ZERO c1, ZERO c2, NIL) -> ZERO (func (c1, c2, NIL))
		| (ZERO c1, ZERO c2, ONE NIL) -> ONE (func (c1, c2, NIL))
		| (ZERO c1, ZERO c2, MONE NIL) -> MONE (func (c1, c2, NIL))
		| (ZERO c1, ONE c2, NIL) -> ONE (func (c1, c2, NIL))
		| (ZERO c1, ONE c2, ONE NIL) -> ZERO (func (c1, c2, ONE NIL))
		| (ZERO c1, ONE c2, MONE NIL) -> ZERO (func (c1, c2, NIL))
		| (ZERO c1, MONE c2, NIL) -> MONE (func (c1, c2, NIL))
		| (ZERO c1, MONE c2, ONE NIL) -> ZERO (func (c1, c2, NIL))
		| (ZERO c1, MONE c2, MONE NIL) -> ZERO (func (c1, c2, MONE NIL))
		| (ONE c1, ZERO c2, NIL) -> ONE (func (c1, c2, NIL))
		| (ONE c1, ZERO c2, ONE NIL) -> ZERO (func (c1, c2, ONE NIL))
		| (ONE c1, ZERO c2, MONE NIL) -> ZERO (func (c1, c2, NIL))
		| (ONE c1, ONE c2, NIL) -> ZERO (func (c1, c2, ONE NIL))
		| (ONE c1, ONE c2, ONE NIL) -> ONE (func (c1, c2, ONE NIL))
		| (ONE c1, ONE c2, MONE NIL) -> ONE (func (c1, c2, NIL))
		| (ONE c1, MONE c2, NIL) -> ZERO (func (c1, c2, NIL))
		| (ONE c1, MONE c2, ONE NIL) -> ONE (func (c1, c2, ONE NIL))
		| (ONE c1, MONE c2, MONE NIL) -> MONE (func (c1, c2, NIL))
		| (MONE c1, ZERO c2, NIL) -> MONE (func (c1, c2, NIL))
		| (MONE c1, ZERO c2, ONE NIL) -> ZERO (func (c1, c2, NIL))
		| (MONE c1, ZERO c2, MONE NIL) -> ZERO (func (c1, c2, MONE NIL))
		| (MONE c1, ONE c2, NIL) -> ZERO (func (c1, c2, NIL))
		| (MONE c1, ONE c2, ONE NIL) -> ONE (func (c1, c2, NIL))
		| (MONE c1, ONE c2, MONE NIL) -> MONE (func (c1, c2, NIL))
		| (MONE c1, MONE c2, NIL) -> ZERO (func (c1, c2, MONE NIL))
		| (MONE c1, MONE c2, ONE NIL) -> MONE (func (c1, c2, NIL))
		| (MONE c1, MONE c2, MONE NIL) -> MONE (func (c1, c2, MONE NIL)) 
		
		in
		
		(func (n1, n2, NIL))
