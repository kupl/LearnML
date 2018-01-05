type crazy2 = 
  NIL 
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val c = match c with 
  NIL -> 0
  | ZERO cr -> 2 * crazy2val cr 
  | ONE cr -> 1 + 2 * crazy2val cr
  | MONE cr -> -1 + 2 * crazy2val cr
