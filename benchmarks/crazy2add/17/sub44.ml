type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2add ((a : crazy2),(b : crazy2)) : crazy2 =
  match a with
  | NIL -> b
  | ZERO atl ->
  (
    match b with
    | NIL -> a
    | ZERO btl -> ZERO (crazy2add(atl, btl))
    | ONE btl -> ONE (crazy2add(atl, btl))
    | MONE btl -> MONE (crazy2add(atl, btl))
  )
  | ONE atl ->
  (
    match b with
    | NIL -> a
    | ZERO btl -> ONE (crazy2add(atl, btl))
    | ONE btl -> ZERO (crazy2add(ONE(NIL),(crazy2add(atl, btl))))
    | MONE btl -> ZERO (crazy2add(atl, btl))
  )
  | MONE atl ->
  (
    match b with
    | NIL -> a
    | ZERO btl -> MONE (crazy2add(atl, btl))
    | ONE btl -> ZERO (crazy2add(atl, btl))
    | MONE btl -> ZERO (crazy2add(MONE(NIL),(crazy2add(atl, btl))))
  )