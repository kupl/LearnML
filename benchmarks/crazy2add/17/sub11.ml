type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (c:crazy2) :int =
  match c with
  |NIL -> 0
  |ZERO(head) -> 0 + (2 * (crazy2val head))
  |ONE(head) -> 1 + (2 * (crazy2val head))
  |MONE(head) -> (- 1) + (2 * (crazy2val head))

let rec crazy2add ((c1:crazy2), (c2:crazy2)):crazy2 =
  match (c1, c2) with
  |(NIL, NIL) -> NIL
  |(c1, NIL) -> c1
  |(NIL, c2) -> c2
  |(ZERO(h1), ZERO(h2)) -> ZERO(crazy2add(h1, h2))
  |(ZERO(h1), ONE(h2)) -> ONE(crazy2add(h1,h2))
  |(ZERO(h1), MONE(h2)) -> MONE(crazy2add(h1,h2))
  |(ONE(h1), ZERO(h2)) -> ONE(crazy2add(h1,h2))
  |(ONE(h1), ONE(h2)) -> ZERO(crazy2add(crazy2add(h1,h2), (ONE NIL)))
  |(ONE(h1), MONE(h2)) ->ZERO(crazy2add(h1,h2))
  |(MONE(h1), ZERO(h2)) ->MONE(crazy2add(h1,h2))
  |(MONE(h1), ONE(h2)) -> ZERO(crazy2add(h1,h2))
  |(MONE(h1), MONE(h2)) -> ZERO(crazy2add(crazy2add(h1,h2), (MONE NIL)))