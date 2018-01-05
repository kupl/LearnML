
type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun(c) ->
    match c with
    | NIL -> 0
    | ZERO(k) -> 0 + 2*crazy2val(k)
    | ONE(k) -> 1 + 2*crazy2val(k)
    | MONE(k) -> -1 + 2*crazy2val(k)



(*
let _MINUS_TWO = ZERO(ONE(MONE NIL))
let _ONE = ONE NIL
let _FIVE = ONE(ZERO(ONE NIL))
let _MINUS_NINE = ONE(MONE(ZERO(MONE NIL)))

let pn = fun(n) -> print_endline(string_of_int(crazy2val(n)))

let _ = pn _MINUS_TWO
let _ = pn _ONE
let _ = pn _FIVE
let _ = pn _MINUS_NINE
*)
