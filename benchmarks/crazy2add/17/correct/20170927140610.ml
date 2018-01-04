type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2

let rec crazy2add ((c1 : crazy2), (c2 : crazy2)) : crazy2 = 
    match (c1, c2) with
    | (NIL, _) -> c2
    | (_, NIL) -> c1
    | (ZERO _c1, ZERO _c2) -> ZERO (crazy2add (_c1, _c2))
    | (ZERO _c1, ONE _c2) -> ONE (crazy2add (_c1, _c2))
    | (ZERO _c1, MONE _c2) -> MONE (crazy2add (_c1, _c2))
    | (ONE _c1, ZERO _c2) -> ONE (crazy2add (_c1, _c2))
    | (ONE _c1, ONE _c2) -> ZERO (crazy2add (crazy2add (_c1, _c2), ONE NIL))
    | (ONE _c1, MONE _c2) -> ZERO (crazy2add (_c1, _c2))
    | (MONE _c1, ZERO _c2) -> MONE (crazy2add (_c1, _c2))
    | (MONE _c1, ONE _c2) -> ZERO (crazy2add (_c1, _c2))
    | (MONE _c1, MONE _c2) -> ZERO (crazy2add (crazy2add (_c1, _c2), MONE NIL))

