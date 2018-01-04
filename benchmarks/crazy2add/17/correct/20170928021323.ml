
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val x =
    match x with
    | NIL -> (0)
    | ZERO y -> (2 * crazy2val(y))
    | ONE y -> (2 * crazy2val(y) +1)
    | MONE y -> (2 * crazy2val(y) -1)
    

let rec crazyinc x =
    match x with
    | NIL -> ONE(NIL)
    | ZERO z -> ONE(z)
    | ONE z -> ZERO(crazyinc(z))
    | MONE z -> ZERO(z)

let rec crazydec x =
    match x with
    | NIL -> MONE(NIL)
    | ZERO z -> MONE(z)
    | ONE z -> ZERO(z)
    | MONE z -> ZERO(crazydec(z))

let rec crazy2add(x, y) =
    match x with
    | NIL -> y
    | ZERO z -> begin match y with
                | NIL -> x
                | ZERO w -> ZERO(crazy2add(z,w))
                | ONE w -> ONE(crazy2add(z,w))
                | MONE w -> MONE(crazy2add(z,w))
                end
    | ONE z -> begin match y with
                | NIL -> x
                | ZERO w -> ONE(crazy2add(z,w))
                | ONE w -> ZERO(crazy2add(crazyinc(z),w))
                | MONE w -> ZERO(crazy2add(z,w))
                end
    | MONE z -> begin match y with
                | NIL -> x
                | ZERO w -> MONE(crazy2add(z,w))
                | ONE w -> ZERO(crazy2add(z,w))
                | MONE w -> ZERO(crazy2add(crazydec(z),w))
                end
