(*2006-11720 Kim Eunsol HW1 #7*)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add(a, b) = 
	match a with ZERO c -> (
			match b with ZERO d -> ZERO(crazy2add(c, d))
							|ONE d -> ONE(crazy2add(c, d))
							|MONE d -> MONE(crazy2add(c, d))
							|NIL -> ZERO(c)
			)

	|ONE c -> (
			match b with ZERO d -> ONE(crazy2add(c, d))
				|ONE d -> ZERO(crazy2add(crazy2add(c, ONE NIL), d))
				|MONE d -> ZERO(crazy2add(c, d))
				|NIL -> ONE(c)
			)
	|MONE c -> (
			match b with ZERO d -> MONE(crazy2add(c, d))
				|ONE d -> ZERO(crazy2add(c, d))
				|MONE d -> ZERO(crazy2add(crazy2add(c, ONE NIL), d))
				|NIL -> MONE(c)
			)
	|NIL -> (
			match b with ZERO d -> ZERO(d)
				|ONE d -> ONE(d)
				|MONE d -> MONE(d)
				|NIL -> NIL
			)
