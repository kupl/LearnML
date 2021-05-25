type crazy2 = NIL
             |ZERO of crazy2
             |ONE of crazy2
             |MONE of crazy2

let rec crazy2add (c1,c2) : crazy2 =
    match c1, c2 with
    |NIL,NIL-> NIL
    |ZERO c1,NIL -> ZERO ( crazy2add  (c1,NIL) )
    |ONE c1,NIL -> ONE ( crazy2add (c1,NIL) )
    |MONE c1,NIL -> MONE ( crazy2add (c1,NIL) )
    |NIL, ZERO c2 -> ZERO ( crazy2add (NIL,c2) )
    |ZERO c1, ZERO c2 -> ZERO ( crazy2add (c1,c2) )
    |ONE c1, ZERO c2 -> ONE ( crazy2add (c1,c2) )
    |MONE c1, ZERO c2 -> MONE ( crazy2add (c1,c2) )
    |NIL, ONE c2 -> ONE ( crazy2add (NIL,c2) )
    |ZERO c1, ONE c2 -> ONE ( crazy2add (c1,c2) )
    |ONE c1, ONE c2 -> ZERO ( crazy2add (c1,(crazy2add (c2,(ONE NIL)))) )
    |MONE c1, ONE c2 -> ZERO ( crazy2add (c1,c2) )
    |NIL, MONE c2 -> MONE ( crazy2add (NIL,c2) )
    |ZERO c1, MONE c2 -> MONE ( crazy2add (c1,c2) )
    |ONE c1, MONE c2 -> ZERO ( crazy2add (c1,c2) )
    |MONE c1, MONE c2 -> ZERO ( crazy2add (c1,( crazy2add (c2,(MONE NIL)) )) )
