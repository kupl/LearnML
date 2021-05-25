type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2


let rec crazy2add (x, y) =
    match (x, y) with 
        _, NIL -> x
        | NIL, _ -> y
        | ONE xs, ONE ys -> ZERO (crazy2add ((crazy2add (xs, (ONE NIL))), ys))
        | MONE xs, MONE ys -> ZERO (crazy2add ((crazy2add (xs, (MONE NIL))), ys))
        | ZERO xs, ONE ys | ONE xs, ZERO ys -> ONE (crazy2add (xs, ys))
        | ZERO xs, MONE ys | MONE xs, ZERO ys -> MONE (crazy2add (xs, ys))
        | ONE xs, MONE ys | MONE xs, ONE ys | ZERO xs, ZERO ys -> ZERO (crazy2add (xs, ys))
