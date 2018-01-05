type crazy2 = NIL
    | ZERO of crazy2
    | ONE of crazy2
    | MONE of crazy2

let rec crazy2val (c2 : crazy2) : int =  
    match c2 with
        | ZERO c -> 2 * (crazy2val c)
        | ONE c -> 1 + 2 * (crazy2val c)
        | MONE c -> -1 + 2 * (crazy2val c)
        | NIL -> 0
