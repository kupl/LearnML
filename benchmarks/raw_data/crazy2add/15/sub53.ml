type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (a, b) ->
  match (a, b) with
  | (ZERO c, ZERO d)
  | (ONE c, MONE d)
  | (MONE c, ONE d) -> ZERO(crazy2add (c, d))
  | (NIL, ZERO d) -> ZERO(crazy2add (NIL, d))
  | (ZERO c, NIL) -> ZERO(crazy2add (c, NIL))
  | (ONE c, ONE d) -> crazy2add(ZERO(crazy2add (c, d)), (ZERO (ONE NIL)))
  | (MONE c, MONE d) -> crazy2add(ZERO(crazy2add (c, d)), (ZERO (MONE NIL)))
  | (ZERO c, ONE d)
  | (ONE c, ZERO d) -> ONE(crazy2add (c, d))
  | (NIL, ONE d) -> ONE(crazy2add (NIL, d))
  | (ONE c, NIL) -> ONE(crazy2add (c, NIL))
  | (ZERO c, MONE d)
  | (MONE c, ZERO d) -> MONE(crazy2add (c, d))
  | (MONE c, NIL) -> MONE(crazy2add (c, NIL))
  | (NIL, MONE d) -> MONE(crazy2add (NIL, d))
  | (NIL, NIL) -> NIL
