exception Error of string

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add = function (a1,a2) -> let sum c1 c2 =
					match (c1,c2) with
  					(ZERO NIL, ZERO NIL) -> ZERO NIL
					|(NIL,_) -> raise(Error "Invalid arg")
					|(_,NIL) -> raise(Error "Invalid arg")
					|(ZERO NIL, _) -> c2
					|(_, ZERO NIL) -> c1
					|(MONE NIL, MONE NIL) -> ZERO(MONE NIL)
					|(MONE NIL, ONE NIL) -> ZERO(NIL)
					|(ONE NIL, ONE NIL) -> ZERO(ONE NIL)
					|(ONE NIL, MONE NIL) -> ZERO(NIL)

					|(ONE NIL, ONE d2) -> ZERO(crazy2add(ONE NIL, d2))
					|(ONE NIL, ZERO d2) -> ONE(d2)
					|(ONE NIL, MONE d2) -> ZERO(d2)
					|(ONE d1, ONE NIL) ->  ZERO(crazy2add(d1, ONE NIL))
					|(ZERO d1, ONE NIL) -> ONE(d1)
					|(MONE d1, ONE NIL) -> ZERO(d1)
					|(ONE d1, MONE NIL) -> ZERO(d1)
					|(ZERO d1, MONE NIL) -> ONE(crazy2add(MONE NIL, d1))
					|(MONE d1, MONE NIL) -> ZERO(crazy2add(d1, MONE NIL))
					|(MONE NIL, ZERO d2) -> MONE(d2)
					|(MONE NIL, ONE d2) -> ZERO(d2)
					|(MONE NIL, MONE d2) -> ZERO(crazy2add(MONE NIL, d2))

  					|(ZERO d1, ONE d2) -> ONE (crazy2add (d1,d2))
 					|(ONE d1, ZERO d2) -> ONE (crazy2add (d1,d2))
  					|(MONE d1, ZERO d2) -> MONE (crazy2add (d1,d2))
  					|(ZERO d1, ZERO d2) -> ZERO (crazy2add (d1,d2))
  					|(ONE d1, ONE d2) -> ZERO (crazy2add((ONE NIL), (crazy2add(d1,d2))))
  					|(MONE d1, MONE d2) -> ZERO (crazy2add ((MONE NIL), (crazy2add(d1,d2))))
  					|(ONE d1, MONE d2) -> ZERO(crazy2add (d1,d2))
					|(MONE d1, ONE d2) -> ZERO(crazy2add (d1,d2))
					|(ZERO d1, MONE d2) -> ONE(crazy2add((MONE NIL), (crazy2add(d1,d2)))) in
					
		let rec check s1 = match s1 with
			NIL -> raise(Error "Invalid arg")
			|ZERO NIL -> NIL
			|ONE NIL -> ONE NIL
			|MONE NIL -> MONE NIL
			|ZERO t1 -> if (check t1) = NIL then NIL else if (check t1) = ONE NIL then ZERO (ONE NIL) else if (check t1) = MONE NIL then ZERO(MONE NIL) else ZERO (check t1)
			|ONE t1 -> ONE (check t1)
			|MONE t1 -> MONE (check t1) in
					if check (sum a1 a2) = NIL then ZERO NIL else check(sum a1 a2);;