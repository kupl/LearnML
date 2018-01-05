type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2valcnt (c : crazy2) (i : int) : int =
    match c with
    | NIL -> 0
    | ZERO (cm) -> 0 * i + (crazy2valcnt cm 2*i)
    | ONE (cm) -> 1 * i + (crazy2valcnt cm 2*i)
    | MONE (cm) -> (-1) * i + (crazy2valcnt cm 2*i)

let rec crazy2val (c : crazy2) : int =
    crazy2valcnt c 1