type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val c2 =
  match c2 with
  | NIL -> 0
  | ZERO c2_pre -> 2 * crazy2val c2_pre
  | ONE c2_pre -> 1 + 2 * crazy2val c2_pre
  | MONE c2_pre -> -1 + 2 * crazy2val c2_pre
