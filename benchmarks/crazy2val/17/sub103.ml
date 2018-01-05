(* Dept. of Computer Science and Engineering, 2015-12055, An Dantae, 2-2 *)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val: crazy2 -> int = fun (a) ->
    match a with
    | NIL -> 0
    | ZERO b -> 2 * crazy2val(b)
    | ONE b -> 1 + 2 * crazy2val(b)
    | MONE b -> -1 + 2 * crazy2val(b)
(* Test Code
let x : crazy2 = ZERO(ONE(MONE NIL))
let y : crazy2 = ONE(ZERO(ONE NIL))
let z : crazy2 = ONE(MONE(ZERO(MONE NIL)))

let test a = print_endline (string_of_int(crazy2val(a)))

let _ = test x
let _ = test y
let _ = test z
*)
