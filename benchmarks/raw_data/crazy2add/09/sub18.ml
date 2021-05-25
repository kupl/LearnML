type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (n1, n2) =
	match n1 with
		(ZERO n3) -> (match n2 with
					(ZERO n4) -> ZERO (crazy2add (n3, n4))
					|(ONE n4) -> ONE (crazy2add (n3, n4))
					|(MONE n4) -> MONE (crazy2add (n3, n4)) 
					| NIL -> ZERO n3 )
		|(ONE n3) -> (match n2 with
					(ZERO n4) -> ONE (crazy2add(n3, n4))
					|(ONE n4) -> ZERO (crazy2add((crazy2add (n3, ONE NIL)), n4))
					|(MONE n4) -> ZERO (crazy2add(n3, n4))
					| NIL -> ONE n3 )
		|(MONE n3) -> (match n2 with
					(ZERO n4) -> MONE (crazy2add(n3, n4))
					|(ONE n4) -> ZERO (crazy2add (n3, n4))
					|(MONE n4) -> ZERO (crazy2add((crazy2add (n3, MONE NIL)), n4))
					| NIL -> ONE n3 )
		|NIL -> n2
