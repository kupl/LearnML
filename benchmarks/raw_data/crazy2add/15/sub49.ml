
type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 =
  fun (c1, c2) ->
    match (c1, c2) with
    | (_, NIL) -> c1
    | (NIL, _) -> c2
    | (ZERO n1, ZERO n2) -> ZERO (crazy2add (n1, n2))
    | (ZERO n1, ONE n2) -> ONE (crazy2add (n1, n2))
    | (ZERO n1, MONE n2) -> MONE (crazy2add (n1, n2))
    | (ONE n1, ZERO n2) -> ONE (crazy2add (n1, n2))
    | (ONE n1, ONE n2) -> ZERO (crazy2add (ONE NIL, crazy2add (n1, n2)))
    | (ONE n1, MONE n2) -> ZERO (crazy2add (n1, n2))
    | (MONE n1, ZERO n2) -> MONE (crazy2add (n1, n2))
    | (MONE n1, ONE n2) -> ZERO (crazy2add (n1, n2))
    | (MONE n1, MONE n2) -> ZERO (crazy2add (MONE NIL, crazy2add (n1, n2)))

