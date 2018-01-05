type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

exception Error of string

let rec crazy2val (crazy2) =

    if crazy2=NIL then raise (Error "Invalid input : NIL")
    else
    let rec doubleval(crazy2) =
        match crazy2 with
          
          ZERO (a) -> 2 * (0 + doubleval(a))
         |ONE (b) -> 2 * (1 + doubleval(b))
         |MONE (c) -> 2 * (-1 + doubleval(c))
         |NIL -> 0
    in

    doubleval(crazy2) / 2 ;;