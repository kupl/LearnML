type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add ((icr1 : crazy2),(icr2 : crazy2)) : crazy2 = 
match (icr1, icr2) with
|(NIL,NIL) -> NIL
|(ZERO cr1, ZERO cr2) -> ZERO (crazy2add(cr1,cr2))
|(ZERO cr1, ONE cr2) -> ONE(crazy2add(cr1, cr2))
|(ZERO cr1, MONE cr2) -> MONE(crazy2add(cr1,cr2))
|(ZERO cr1, NIL) -> ZERO(crazy2add(cr1,NIL))
|(ONE cr1, ZERO cr2) -> ONE(crazy2add(cr1,cr2))
|(ONE cr1, ONE cr2) -> ZERO(crazy2add((crazy2add(cr1,cr2)),ONE NIL))
|(ONE cr1, MONE cr2) -> ZERO(crazy2add(cr1, cr2))
|(ONE cr1, NIL) -> ONE(crazy2add(cr1,NIL))
|(MONE cr1, ZERO cr2) -> MONE(crazy2add(cr1,cr2))
|(MONE cr1, ONE cr2) -> ZERO(crazy2add(cr1,cr2))
|(MONE cr1, MONE cr2) -> ZERO(crazy2add(crazy2add(cr1, cr2),MONE NIL))
|(MONE cr1, NIL) -> MONE(crazy2add(cr1,NIL))
|(NIL, ZERO cr2) -> ZERO(crazy2add(cr2,NIL))
|(NIL, ONE cr2) -> ONE(crazy2add(cr2,NIL))
|(NIL, MONE cr2) -> MONE(crazy2add(cr2,NIL))

