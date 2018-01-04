
type crazy2 = NIL
			 |ZERO of crazy2
			 |ONE of crazy2
			 |MONE of crazy2

let rec crazy2add ((c1 : crazy2), (c2 : crazy2)) : crazy2 =
										   match (c1,c2) with
															 (NIL,_) -> c2
															|(_,NIL) -> c1
															|(ZERO(crazy_1),ZERO(crazy_2)) |(MONE(crazy_1),ONE(crazy_2)) |(ONE(crazy_1),MONE(crazy_2)) -> ZERO(crazy2add(crazy_1,crazy_2))
															|(ONE(crazy_1),ZERO(crazy_2)) |(ZERO(crazy_1),ONE(crazy_2)) -> ONE(crazy2add(crazy_1,crazy_2))
															|(MONE(crazy_1),ZERO(crazy_2)) |(ZERO(crazy_1),MONE(crazy_2)) -> MONE(crazy2add(crazy_1,crazy_2))
															|(ONE(crazy_1),ONE(crazy_2)) -> ZERO(crazy2add(crazy2add(crazy_1,crazy_2),ONE(NIL)))
															|(MONE(crazy_1),MONE(crazy_2)) -> ZERO(crazy2add(crazy2add(crazy_1,crazy_2),MONE(NIL)))

