type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add((x:crazy2),(y:crazy2)): crazy2 =
  match (x,y) with
  | (NIL, NIL) -> NIL
  | (NIL, a) -> a
  | (a, NIL) -> a
  | (ZERO(y), ZERO(z)) -> ZERO(crazy2add(y, z))
  | (ZERO(y), ONE(z)) -> ONE(crazy2add(y,z))
  | (ZERO(y), MONE(z)) -> MONE(crazy2add(y,z))
  | (ONE(y), ZERO(z)) -> ONE(crazy2add(y, z))
  | (ONE(y), ONE(z)) -> ZERO(crazy2add(ONE(NIL),crazy2add(y,z)))
  | (ONE(y), MONE(z)) -> ZERO(crazy2add(y,z))
  | (MONE(y), ZERO(z)) -> MONE(crazy2add(y,z))
  | (MONE(y), ONE(z)) -> ZERO(crazy2add(y,z))
  | (MONE(y), MONE(z)) -> ZERO(crazy2add(MONE(NIL),crazy2add(y,z)))

  
