type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val target =
  match target with
    NIL -> 0
  | ZERO next -> 2 * crazy2val next
  | ONE next -> 1 + 2 * crazy2val next
  | MONE next -> -1 + 2 * crazy2val next
