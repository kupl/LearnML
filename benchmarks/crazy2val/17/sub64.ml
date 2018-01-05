type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val (n: crazy2) : int =
  match n with
  | ZERO m -> 2 * (crazy2val m)
  | ONE m -> 1 + 2 * (crazy2val m)
  | MONE m -> -1 + 2 * (crazy2val m)
  | NIL -> 0
