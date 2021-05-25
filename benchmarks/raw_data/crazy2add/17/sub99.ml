type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2


let rec crazy2add (a, b) = match a, b with
    | _, NIL -> a
    | NIL, _ -> b
    | ZERO(a'), ZERO(b') -> ZERO(crazy2add (a', b'))
    | ZERO(a'), ONE(b') -> ONE(crazy2add (a', b'))
    | ZERO(a'), MONE(b') -> MONE(crazy2add (a', b'))
    | ONE(a'), ZERO(b') -> ONE(crazy2add (a', b'))
    | ONE(a'), ONE(b') -> ZERO(crazy2add (crazy2add (a', b'), ONE(NIL)))
    | ONE(a'), MONE(b') -> ZERO(crazy2add (a', b'))
    | MONE(a'), ZERO(b') -> MONE(crazy2add (a', b'))
    | MONE(a'), ONE(b') -> ZERO(crazy2add (a', b'))
    | MONE(a'), MONE(b') -> ZERO(crazy2add (crazy2add (a', b'), MONE(NIL)))
