type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2

let rec crazy2val (c : crazy2) : int =
    match c with
    | NIL -> 0
    | ZERO _c -> 2 * (crazy2val _c)
    | ONE _c -> 1 + 2 * (crazy2val _c)
    | MONE _c -> -1 + 2 * (crazy2val _c)

