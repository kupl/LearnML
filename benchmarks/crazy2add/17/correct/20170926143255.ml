type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (a, b) : crazy2 = 
    match (a, b) with
    | (NIL, _) -> b
    | (_, NIL) -> a
    | (ZERO c1, ZERO c2) -> ZERO (crazy2add(c1, c2))
    | (ZERO c1, MONE c2) -> MONE (crazy2add(c1, c2))
    | (ZERO c1, ONE c2) -> ONE (crazy2add(c1, c2))
    | (MONE c1, ZERO c2) -> MONE (crazy2add(c1, c2))
    | (MONE c1, MONE c2) -> ZERO (crazy2add(crazy2add(c1, c2), MONE NIL))
    | (MONE c1, ONE c2) -> ZERO (crazy2add(c1, c2))
    | (ONE c1, ZERO c2) -> ONE (crazy2add(c1, c2))
    | (ONE c1, MONE c2) -> ZERO (crazy2add(c1, c2))
    | (ONE c1, ONE c2) -> ZERO (crazy2add(crazy2add(c1, c2), ONE NIL))


(*let rec crazy2val c : int =
    match c with
    | NIL -> 0
    | ZERO c2 -> 2 * (crazy2val c2)
    | ONE c2 -> 1 + 2 * (crazy2val c2)
    | MONE c2 -> 2 * (crazy2val c2) - 1*)
