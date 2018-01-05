type crazy2 = 
NIL
|ZERO of crazy2
|ONE of crazy2
|MONE of crazy2;;

let rec crazy2val input =
 match input with
 |NIL -> 0
 |ZERO (r) -> 2 * (crazy2val r)
 |ONE (r) -> 1 + 2 * (crazy2val r)
 |MONE (r) -> -1 + 2 * (crazy2val r)
;;
