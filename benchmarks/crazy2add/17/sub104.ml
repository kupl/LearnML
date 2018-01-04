type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add ((a:crazy2), (b:crazy2)) : crazy2 =
    match (a, b) with
    | (NIL, y) -> y
    | (x, NIL) -> x
    | (ONE x, ONE y) -> ZERO(crazy2add(crazy2add(x,y),ONE NIL))
    | (ONE x, ZERO y) -> ONE(crazy2add(x,y))
    | (ONE x, MONE y) -> ZERO(crazy2add(x,y))
    | (MONE x, ONE y) -> ZERO(crazy2add(x,y))
    | (MONE x, ZERO y) -> MONE(crazy2add(x,y))
    | (MONE x, MONE y) -> ZERO(crazy2add(crazy2add(x,y),MONE NIL))
    | (ZERO x, ONE y) -> ONE(crazy2add(x,y))
    | (ZERO x, ZERO y) -> ZERO(crazy2add(x,y))
    | (ZERO x, MONE y) -> MONE(crazy2add(x,y))
