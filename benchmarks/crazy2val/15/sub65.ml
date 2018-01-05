type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val a = match a with
  |NIL -> 0
  |ZERO k -> 2 * crazy2val k
  |ONE k -> 2 * crazy2val k + 1
  |MONE k -> 2 * crazy2val k - 1


