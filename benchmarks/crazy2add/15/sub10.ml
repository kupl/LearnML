
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



(*
let rec crazy2val : crazy2 -> int = fun(c) ->
    match c with
    | NIL -> 0
    | ZERO(k) -> 0 + 2*crazy2val(k)
    | ONE(k) -> 1 + 2*crazy2val(k)
    | MONE(k) -> -1 + 2*crazy2val(k)


let _MINUS_TWO = ZERO(ONE(MONE NIL))
let _ONE = ONE NIL
let _FIVE = ONE(ZERO(ONE NIL))
let _MINUS_NINE = ONE(MONE(ZERO(MONE NIL)))
let _ZERO = NIL

let pn = fun(n) -> print_endline(string_of_int(crazy2val(n)))

let _ = pn(_ZERO)
let _ = pn(crazy2add(_ZERO, _ZERO))
let _ = pn(crazy2add(_MINUS_TWO, _ONE))
let _ = pn(crazy2add(_MINUS_NINE, _MINUS_TWO))
let _ = pn(crazy2add(_FIVE, _MINUS_NINE))
let _ = pn(crazy2add(crazy2add(_ONE, _FIVE), _FIVE))

let _MINUS_NINETEEN = crazy2add(_MINUS_NINE, _MINUS_NINE)
let _ = pn(crazy2add(_MINUS_NINETEEN, _MINUS_TWO))

*)



