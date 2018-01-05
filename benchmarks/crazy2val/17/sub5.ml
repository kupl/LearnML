type crazy2 = NIL |
              ZERO of crazy2 |
              ONE of crazy2 |
              MONE of crazy2

let rec crazy2val : crazy2 -> int = fun c ->
  match c with
  | NIL -> 0
  | ZERO c_t -> 0 + 2*(crazy2val c_t)
  | ONE c_t -> 1 + 2*(crazy2val c_t)
  | MONE c_t -> -1 + 2*(crazy2val c_t)
