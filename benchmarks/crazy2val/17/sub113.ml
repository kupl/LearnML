type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val c : int =
    match c with
    | NIL -> 0
    | ZERO c2 -> 2 * (crazy2val c2)
    | ONE c2 -> 1 + 2 * (crazy2val c2)
    | MONE c2 -> 2 * (crazy2val c2) - 1

(*let _ = print_endline(string_of_int (crazy2val (ONE NIL)))
let _ = print_endline(string_of_int (crazy2val (ONE(ZERO(ONE NIL)))))
let _ = print_endline(string_of_int (crazy2val (ONE(MONE NIL))))
let _ = print_endline(string_of_int (crazy2val (ONE(MONE(ZERO(MONE NIL))))))*)
