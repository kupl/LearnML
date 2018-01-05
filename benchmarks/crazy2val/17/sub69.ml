type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val crz2 =
  match crz2 with
  | NIL -> 0
  | ZERO a -> (crazy2val a)*2
  | ONE a -> (crazy2val a)*2 + 1
  | MONE a -> (crazy2val a)*2 - 1

