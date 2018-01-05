type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun(c) ->
        match c with
        | NIL -> 0
        | ZERO c1 -> crazy2val(c1)*2
        | ONE c2 -> 1+crazy2val(c2)*2
        | MONE c3 -> -1+crazy2val(c3)*2
