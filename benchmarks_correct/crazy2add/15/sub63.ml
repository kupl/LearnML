type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (a,b) = match a with
  |NIL -> b
  |ZERO k -> (match b with
    |NIL -> a
    |ZERO q -> ZERO(crazy2add (k,q))
    |ONE q -> ONE(crazy2add (k,q))
    |MONE q -> MONE(crazy2add (k,q))
    )
  |ONE k -> (match b with
    |NIL -> a
    |ZERO q -> ONE(crazy2add (k,q))
    |ONE q -> ZERO(crazy2add (crazy2add (k,q), ONE(NIL)))
    |MONE q -> ZERO(crazy2add (k,q))
    )
  |MONE k -> (match b with
    |NIL -> a 
    |ZERO q -> MONE(crazy2add (k,q))
    |ONE q -> ZERO(crazy2add (k,q))
    |MONE q -> ZERO(crazy2add (crazy2add(k,q), MONE(NIL))) 
    )


