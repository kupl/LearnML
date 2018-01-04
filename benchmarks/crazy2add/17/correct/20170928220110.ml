type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add ((c1 : crazy2), (c2 : crazy2)) : crazy2 =
	match(c1, c2) with
	|(_, NIL) -> c1
	|(NIL, _) -> c2
	|(ZERO(c1_next), ZERO(c2_next)) -> ZERO(crazy2add(c1_next,c2_next))
	|(ZERO(c1_next), ONE(c2_next)) -> ONE(crazy2add(c1_next,c2_next))
	|(ZERO(c1_next), MONE(c2_next)) -> MONE(crazy2add(c1_next,c2_next))
	|(ONE(c1_next), ZERO(c2_next)) -> ONE(crazy2add(c1_next,c2_next))
	|(ONE(c1_next), ONE(c2_next)) -> ZERO(crazy2add(crazy2add(c1_next,c2_next), ONE(NIL)))
	|(ONE(c1_next), MONE(c2_next)) -> ZERO(crazy2add(c1_next,c2_next))
	|(MONE(c1_next), ZERO(c2_next)) -> MONE(crazy2add(c1_next,c2_next))
	|(MONE(c1_next), ONE(c2_next)) -> ZERO(crazy2add(c1_next,c2_next))
	|(MONE(c1_next), MONE(c2_next)) -> ZERO(crazy2add(crazy2add(c1_next,c2_next), MONE(NIL)))