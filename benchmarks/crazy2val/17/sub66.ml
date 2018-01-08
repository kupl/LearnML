type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val (c: crazy2): int =
  match c with
    | NIL -> 0
    | ZERO(c1) -> 2*(crazy2val c1)
    | ONE(c1) -> 2*(crazy2val c1) + 1
    | MONE(c1) -> 2*(crazy2val c1) - 1