type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (a, b) ->
    match (a, b) with
    | (a, NIL) -> a
    | (NIL, b) -> b
    | (ZERO(c), ZERO(d))
    | (ONE(c), MONE(d))
    | (MONE(c), ONE(d)) -> (ZERO (crazy2add(c, d)))
    | (ZERO(c), ONE(d))
    | (ONE(c), ZERO(d)) -> (ONE (crazy2add(c, d)))
    | (ZERO(c), MONE(d))
    | (MONE(c), ZERO(d)) -> (MONE (crazy2add(c, d)))
    | (ONE c, ONE d) -> (ZERO (crazy2add (crazy2add (c, d), ONE NIL)))
    | (MONE c, MONE d) -> (ZERO (crazy2add (crazy2add (c, d), MONE NIL)))
    | (NIL, ONE(c))
    | (ONE(c), NIL) -> (ONE (crazy2add(c, NIL)))
    | (NIL, MONE(c))
    | (MONE(c), NIL) -> (MONE (crazy2add(c, NIL)))
    | (ZERO(c), NIL)
    | (NIL, ZERO(c)) -> (ZERO (crazy2add(c, NIL)))
