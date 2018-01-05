type crazy2 = NIL
             |ZERO of crazy2
             |ONE of crazy2
             |MONE of crazy2

let rec crazy2val c2 : int =
    match c2 with
    |NIL->0
    |ZERO c -> (crazy2val c) * 2
    |ONE c -> (crazy2val c) * 2 + 1
    |MONE c -> (crazy2val c) * 2 - 1
