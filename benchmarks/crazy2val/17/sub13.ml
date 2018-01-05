type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (c:crazy2) :int =
  match c with
  |NIL -> 0
  |ZERO(head) -> 0 + (2 * (crazy2val head))
  |ONE(head) -> 1 + (2 * (crazy2val head))
  |MONE(head) -> (- 1) + (2 * (crazy2val head))

  