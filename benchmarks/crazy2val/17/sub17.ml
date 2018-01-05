type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val t = 
  match t with
  |NIL -> 0
  |ZERO t2 -> 2 * crazy2val t2
  |ONE t2 -> 1 + 2 * crazy2val t2
  |MONE t2 -> -1 + 2 * crazy2val t2

