type crazy2 =
  |NIL
  |ZERO of crazy2
  |ONE of crazy2
  |MONE of crazy2;;

let rec crazy2add (x1, x2) =
  match x1 with
  |NIL -> x2
  |ZERO a -> (match x2 with
              |NIL -> x1
              |ZERO b -> ZERO (crazy2add (a, b))
              |ONE b -> ONE (crazy2add (a, b))
              |MONE b -> MONE (crazy2add (a, b)))
  |ONE a -> (match x2 with
              |NIL -> x1
              |ZERO b -> ONE (crazy2add (a, b))
              |ONE b -> crazy2add (ZERO (ONE NIL), ZERO (crazy2add (a, b)))
              |MONE b -> ZERO (crazy2add (a, b)))
  |MONE a -> (match x2 with
              |NIL -> x1
              |ZERO b -> MONE (crazy2add (a, b))
              |ONE b -> ZERO (crazy2add (a, b))
              |MONE b -> crazy2add (ZERO (MONE NIL), ZERO (crazy2add (a, b))))


