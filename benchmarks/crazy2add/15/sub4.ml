type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (x,y) ->
match (x,y) with
    (NIL, y) -> y
    | (x, NIL) -> x
    | (ZERO x2, ZERO y2) -> ZERO(crazy2add(x2,y2) )
    | (ZERO x2, ONE y2) -> ONE(crazy2add(x2,y2))
    | (ZERO x2, MONE y2) -> MONE(crazy2add(x2,y2))
    | (ONE x2, ZERO y2) -> ONE(crazy2add(x2,y2))
    | (MONE x2, ZERO y2) -> MONE(crazy2add(x2,y2))
    | (ONE x2, MONE y2) -> ZERO(crazy2add(x2,y2))
    | (MONE x2, ONE y2) -> ZERO(crazy2add(x2,y2))
    | (ONE x2, ONE y2) -> ZERO(crazy2add(ONE(NIL),crazy2add(x2,y2)))
    | (MONE x2, MONE y2) -> ZERO(crazy2add(MONE(NIL),crazy2add(x2,y2))) ;;


