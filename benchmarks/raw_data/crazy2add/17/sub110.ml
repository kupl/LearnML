(* 2015-1478 Giyeon Kim HW 2 *)

(* Exercise 3 *)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (lcnum, rcnum) ->
    match (lcnum, rcnum) with
    | (NIL, _) -> rcnum
    | (_, NIL) -> lcnum
    | (ZERO t1, ZERO t2) -> ZERO (crazy2add (t1, t2))
    | (ZERO t1, ONE t2) -> ONE (crazy2add (t1, t2))
    | (ZERO t1, MONE t2) -> MONE (crazy2add (t1, t2))
    | (ONE t1, ZERO t2) -> ONE (crazy2add (t1, t2))
    | (ONE t1, ONE t2) -> ZERO (crazy2add ((ONE NIL), (crazy2add (t1, t2))))
    | (ONE t1, MONE t2) -> ZERO (crazy2add (t1, t2))
    | (MONE t1, ZERO t2) -> MONE (crazy2add (t1, t2))
    | (MONE t1, ONE t2) -> ZERO (crazy2add (t1, t2))
    | (MONE t1, MONE t2) -> ZERO (crazy2add ((MONE NIL), (crazy2add (t1, t2))))


