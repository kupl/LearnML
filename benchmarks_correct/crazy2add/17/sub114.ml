type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (a, b) ->
  match (a, b) with
  | (NIL, _) -> b
  | (_, NIL) -> a
  | (ZERO a2, ZERO b2) -> ZERO (crazy2add(a2, b2))
  | (ONE  a2, ONE  b2) -> ZERO (crazy2add(a2, crazy2add(b2, ONE NIL)))
  | (MONE a2, MONE b2) -> ZERO (crazy2add(a2, crazy2add(b2, MONE NIL)))
  | (ZERO a2, ONE  b2) | (ONE  a2, ZERO b2) -> ONE  (crazy2add(a2, b2))
  | (ZERO a2, MONE b2) | (MONE a2, ZERO b2) -> MONE (crazy2add(a2, b2))
  | (ONE  a2, MONE b2) | (MONE a2, ONE  b2) -> ZERO (crazy2add(a2, b2))