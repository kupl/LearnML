
type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2


let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun(c1, c2) ->
    match c1, c2 with
    | NIL, a -> a
    | a, NIL -> a
    | ZERO(k1), ZERO(k2) -> ZERO(crazy2add(k1, k2))
    | ZERO(k1), ONE(k2) -> ONE(crazy2add(k1, k2))
    | ZERO(k1), MONE(k2) -> MONE(crazy2add(k1, k2))
    | ONE(k1), ZERO(k2) -> ONE(crazy2add(k1, k2))
    | ONE(k1), ONE(k2) -> ZERO(crazy2add(crazy2add(ONE(NIL), k1), k2))
    | ONE(k1), MONE(k2) -> ZERO(crazy2add(k1, k2))
    | MONE(k1), ZERO(k2) -> MONE(crazy2add(k1, k2))
    | MONE(k1), ONE(k2) -> ZERO(crazy2add(k1, k2))
    | MONE(k1), MONE(k2) -> ZERO(crazy2add(crazy2add(MONE(NIL), k1), k2))

