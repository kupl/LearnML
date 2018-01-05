type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val a =
  match a with
  | NIL -> 0
  | ZERO b -> 2*(crazy2val b)
  | ONE b -> 1 + 2*(crazy2val b)
  | MONE b -> -1 + 2*(crazy2val b)
