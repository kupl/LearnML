type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val (x: crazy2): int = 
  match x with
  | NIL -> 0
  | ZERO (y) -> (2 * crazy2val (y))
  | ONE (z) -> (1 + 2 * crazy2val (z))
  | MONE (w) -> (-1 + 2 * crazy2val (w))