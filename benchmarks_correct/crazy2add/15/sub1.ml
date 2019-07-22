type crazy2 = NIL
  	|ZERO of crazy2
  	|ONE of crazy2
  	|MONE of crazy2

let rec crazy2add (exp1, exp2) = 
  match exp1 with
  |NIL -> exp2
  |ZERO a -> 
      (match exp2 with
      |NIL -> ZERO a
      |ZERO b -> ZERO (crazy2add(a, b))
      |ONE b -> ONE (crazy2add(a, b))
      |MONE b -> MONE (crazy2add (a, b)))
  |ONE a ->
      (match exp2 with
      |NIL -> ONE a
      |ZERO b -> ONE (crazy2add(a, b))
      |ONE b -> ZERO (crazy2add ((ONE NIL), (crazy2add(a, b))))
      |MONE b -> ZERO (crazy2add (a, b)))
  |MONE a -> 
      (match exp2 with
      |NIL -> MONE a
      |ZERO b -> MONE (crazy2add(a, b))
      |ONE b -> ZERO (crazy2add (a, b))
      |MONE b -> ZERO (crazy2add ((MONE NIL), (crazy2add(a, b)))))
