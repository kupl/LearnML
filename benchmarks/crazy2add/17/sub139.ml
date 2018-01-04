(* 컴퓨터공학과/2017-34165/김성국/2-3 *)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (x, y) =
  let one = ONE(NIL)
  and mone = MONE(NIL) in
  match (x, y) with
  | (NIL, y) -> y
  | (x, NIL) -> x
  | (ZERO xx, ZERO yy) -> ZERO((crazy2add (xx, yy)))
  | (ZERO xx, ONE yy) -> ONE((crazy2add (xx, yy)))
  | (ZERO xx, MONE yy) -> MONE((crazy2add (xx, yy)))
  | (ONE xx, ZERO yy) -> ONE((crazy2add (xx, yy)))
  | (ONE xx, ONE yy) -> ZERO(crazy2add (one, (crazy2add (xx, yy))))
  | (ONE xx, MONE yy) -> ZERO((crazy2add (xx, yy)))
  | (MONE xx, ZERO yy) -> MONE((crazy2add (xx, yy)))
  | (MONE xx, ONE yy) -> ZERO((crazy2add (xx, yy)))
  | (MONE xx, MONE yy) -> ZERO(crazy2add (mone, (crazy2add (xx, yy))))
