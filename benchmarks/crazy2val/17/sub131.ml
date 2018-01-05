type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun cra -> 
  match cra with
    NIL -> 0
  | ZERO cra1 -> 0 + 2 * crazy2val cra1
  | ONE cra1 -> 1 + 2* crazy2val cra1
  | MONE cra1 -> -1 + 2 * crazy2val cra1