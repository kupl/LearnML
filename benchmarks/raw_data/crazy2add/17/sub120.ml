type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun c ->
    match c with
    |(NIL, a) -> a
    |(a, NIL) -> a
    |(MONE a, MONE b) -> ZERO(crazy2add(crazy2add(a,b) , MONE NIL))
    |(ONE a, ONE b) -> ZERO(crazy2add(crazy2add(a,b) , ONE NIL))
    |(ZERO a, ZERO b) -> ZERO(crazy2add(a,b))
    |(ZERO a, MONE b) -> MONE(crazy2add(a,b))
    |(MONE a, ZERO b ) -> MONE(crazy2add(a,b))
    |(ZERO a, ONE b) -> ONE(crazy2add(a,b))
    |(ONE a, ZERO b) -> ONE(crazy2add(a,b))
    |(ONE a, MONE b) -> ZERO(crazy2add(a,b))
    |(MONE a, ONE b) -> ZERO(crazy2add(a,b))