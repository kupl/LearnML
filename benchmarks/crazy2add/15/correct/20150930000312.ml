type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
let rec crazy2add (a, b) =
  match (a, b) with
  | (NIL, _) -> b
  | (_, NIL) -> a
  | (ZERO aa, ZERO bb) -> ZERO (crazy2add (aa, bb))
  | (ONE aa, MONE bb) -> ZERO (crazy2add (aa, bb))
  | (MONE aa, ONE bb) -> ZERO (crazy2add (aa, bb))
  | (ZERO aa, ONE bb) -> ONE (crazy2add (aa, bb))
  | (ONE aa, ZERO bb) -> ONE (crazy2add (aa, bb))
  | (ZERO aa, MONE bb) -> MONE (crazy2add (aa, bb))
  | (MONE aa, ZERO bb) -> MONE (crazy2add (aa, bb))
  | (ONE aa, ONE bb) -> ZERO (crazy2add (crazy2add (aa, ONE NIL), bb))
  | (MONE aa, MONE bb) -> ZERO (crazy2add (crazy2add (aa, MONE NIL), bb))
