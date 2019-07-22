type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

let rec crazy2add ((c1: crazy2), (c2: crazy2)) : crazy2 =
    match (c1, c2) with
    | (ONE(x), ONE(y)) -> crazy2add((ZERO(ONE(NIL)), ZERO(crazy2add(x, y))))
    | (MONE(x), MONE(y)) -> crazy2add((ZERO(MONE(NIL)), ZERO(crazy2add(x, y))))
    | (MONE(x), ONE(y)) -> ZERO(crazy2add (x, y))
    | (ONE(x), MONE(y)) -> ZERO(crazy2add (x, y))
    | (NIL, _) -> c2
    | (_, NIL) -> c1
    | (ZERO(x), ONE(y)) -> ONE(crazy2add(x, y))
    | (ZERO(x), MONE(y)) -> MONE(crazy2add(x, y))
    | (ONE(x), ZERO(y)) -> ONE(crazy2add(x, y))
    | (MONE(x), ZERO(y)) -> MONE(crazy2add(x, y))
    | (ZERO(x), ZERO(y)) -> ZERO(crazy2add(x, y));;
