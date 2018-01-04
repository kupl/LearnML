type crazy2 =
    | NIL
    | ZERO of crazy2
    | ONE of crazy2
    | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun (c) ->
    match c with
    | NIL -> 0
    | ZERO x -> 2 * (crazy2val x)
    | ONE x -> 1 + 2 * (crazy2val x)
    | MONE x -> -1 + 2 * (crazy2val x)

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (c1, c2) ->
    match (c1, c2) with
    | (_, NIL) -> c1
    | (NIL, _) -> c2
    | (ZERO x1, ZERO x2)    -> ZERO (crazy2add (x1, x2))
    | (ZERO x1, ONE x2)
    | (ONE x1, ZERO x2)     -> ONE (crazy2add (x1, x2))
    | (ZERO x1, MONE x2)
    | (MONE x1, ZERO x2)    -> MONE (crazy2add (x1, x2))
    | (ONE x1, MONE x2)
    | (MONE x1, ONE x2)     -> ZERO (crazy2add (x1, x2))
    | (ONE x1, ONE x2)      -> ZERO (crazy2add (ONE(NIL), crazy2add (x1, x2)))
    | (MONE x1, MONE x2)    -> ZERO (crazy2add (MONE(NIL), crazy2add(x1, x2)))
