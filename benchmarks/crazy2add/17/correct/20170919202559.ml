type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add ((x : crazy2), (y : crazy2)) : crazy2 =
    match x, y with
    | NIL, y -> y
    | x, NIL -> x
    | ONE xtl, ZERO ytl | ZERO xtl, ONE ytl -> ONE (crazy2add (xtl, ytl))
    | MONE xtl, ZERO ytl | ZERO xtl, MONE ytl -> MONE (crazy2add (xtl, ytl))
    | ZERO xtl, ZERO ytl | MONE xtl, ONE ytl | ONE xtl, MONE ytl -> ZERO (crazy2add (xtl, ytl))
    | MONE xtl, MONE ytl -> ZERO (crazy2add (crazy2add (xtl, ytl), MONE NIL))
    | ONE xtl, ONE ytl -> ZERO (crazy2add (crazy2add (xtl, ytl), ONE NIL))
