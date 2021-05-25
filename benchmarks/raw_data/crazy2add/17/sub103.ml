(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-3 *)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

(*
let rec crazy2val: crazy2 -> int = fun (a) ->
    match a with
    | NIL -> 0
    | ZERO b -> 2 * crazy2val(b)
    | ONE b -> 1 + 2 * crazy2val(b)
    | MONE b -> -1 + 2 * crazy2val(b)
*)

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (a, b) ->
    match (a, b) with
    | (NIL, _) -> b
    | (_, NIL) -> a
    | (ZERO a', ZERO b') -> ZERO(crazy2add(a', b'))
    | (ZERO a', ONE b') -> ONE(crazy2add(a', b'))
    | (ZERO a', MONE b') -> MONE(crazy2add(a', b'))
    | (ONE a', ZERO b') -> ONE(crazy2add(a', b'))
    | (ONE a', ONE b') -> ZERO(crazy2add(a', crazy2add(b', ONE NIL)))
    | (ONE a', MONE b') -> ZERO(crazy2add(a', b'))
    | (MONE a', ZERO b') -> MONE(crazy2add(a', b'))
    | (MONE a', ONE b') -> ZERO(crazy2add(a', b'))
    | (MONE a', MONE b') -> ZERO(crazy2add(a', crazy2add(b', MONE NIL)))

(* Test Code
let x : crazy2 = ZERO(ONE(MONE NIL))
let y : crazy2 = ONE(ZERO(ONE NIL))
let z : crazy2 = ONE(MONE(ZERO(MONE NIL)))
let w : crazy2 = ZERO(MONE NIL)
let t : crazy2 = ONE(ONE(ONE(ONE(ONE NIL))))

let test (a, b) = print_endline (string_of_int(crazy2val(crazy2add(a, b))))

let _ = test (x, y)
let _ = test (y, x)
let _ = test (z, x)
let _ = test (y, z)
let _ = test (x, x)
let _ = test (y, y)
let _ = test (z, z)
let _ = test (x, w)
let _ = test (z, w)
let _ = test (t, t)
*)
