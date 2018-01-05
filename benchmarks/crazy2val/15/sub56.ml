type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val: crazy2 -> int = fun c ->
  match c with
  | NIL -> 0
  | ZERO c -> (crazy2val c) * 2
  | ONE c -> (crazy2val c) * 2 + 1
  | MONE c -> (crazy2val c) * 2 - 1
