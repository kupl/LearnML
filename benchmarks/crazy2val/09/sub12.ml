type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val c =
  match c with
    NIL -> 0
  | ZERO n -> 2 * (crazy2val n)
  | ONE n -> 1 + 2 * (crazy2val n)
  | MONE n -> -1 + 2 * (crazy2val n)
