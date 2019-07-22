type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (a,b) =
  match (a,b) with
  |(a,NIL) -> a
  |(NIL,b) -> b
  |(ZERO a2, MONE b2) -> MONE(crazy2add(a2,b2))
  |(ZERO a2, ZERO b2) -> ZERO(crazy2add(a2,b2))
  |(ZERO a2, ONE b2) -> ONE(crazy2add(a2,b2))
  |(MONE a2, ZERO b2) -> MONE(crazy2add(a2,b2))
  |(ONE a2, ZERO b2) -> ONE(crazy2add(a2,b2))
  |(ONE a2, ONE b2) -> ZERO(crazy2add(crazy2add(ONE NIL,a2),b2))
  |(MONE a2, MONE b2) -> ZERO(crazy2add(crazy2add(MONE NIL,a2),b2))
  |(ONE a2, MONE b2) -> ZERO(crazy2add(a2,b2))
  |(MONE a2, ONE b2) -> ZERO(crazy2add(a2,b2))

