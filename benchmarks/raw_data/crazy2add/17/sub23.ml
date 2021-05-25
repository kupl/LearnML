type crazy2 = NIL
| ZERO of crazy2
| ONE of crazy2
| MONE of crazy2

let rec crazy2add((a : crazy2), (b : crazy2)) : crazy2 =
match (a,b) with
|(NIL, _) -> b
|(_, NIL) -> a
|(ZERO a_pre, ZERO b_pre) -> ZERO(crazy2add(a_pre, b_pre))
|(ZERO a_pre, ONE b_pre) -> ONE(crazy2add(a_pre, b_pre))
|(ONE a_pre, ZERO b_pre) -> ONE(crazy2add(a_pre, b_pre))
|(ZERO a_pre, MONE b_pre) -> MONE(crazy2add(a_pre, b_pre))
|(MONE a_pre, ZERO b_pre) -> MONE(crazy2add(a_pre, b_pre))
|(ONE a_pre, MONE b_pre) -> ZERO(crazy2add(a_pre, b_pre))
|(MONE a_pre, ONE b_pre) -> ZERO(crazy2add(a_pre, b_pre))
|(ONE a_pre, ONE b_pre) -> crazy2add(ZERO(crazy2add(a_pre, b_pre)),ZERO(ONE(NIL)))
|(MONE a_pre, MONE b_pre) -> crazy2add(ZERO(crazy2add(a_pre, b_pre)),ZERO(MONE(NIL)))

