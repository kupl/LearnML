type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

let rec crazy2val : crazy2 -> int = fun c -> 
  match c with
  | NIL -> 0
  | ZERO(cin) -> 2*(crazy2val cin)+0
  | ONE(cin) -> 2*(crazy2val cin)+1
  | MONE(cin) -> 2*(crazy2val cin)-1
;;


