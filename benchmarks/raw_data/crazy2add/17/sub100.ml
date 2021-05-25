type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add (cz1,cz2) : crazy2 =
  match (cz1,cz2) with
  | (NIL,cz) -> cz
  | (cz,NIL) -> cz
  | (ZERO c1,ZERO c2) -> ZERO (crazy2add(c1,c2))
  | (ZERO c1,ONE c2) -> ONE (crazy2add(c1,c2))
  | (ONE c1,ZERO c2) -> ONE (crazy2add(c1,c2))
  | (ZERO c1,MONE c2) -> MONE (crazy2add(c1,c2))
  | (MONE c1,ZERO c2) -> MONE (crazy2add(c1,c2))
  | (ONE c1,MONE c2) -> ZERO (crazy2add(c1,c2))
  | (MONE c1,ONE c2) -> ZERO (crazy2add(c1,c2))
  | (ONE c1,ONE c2) -> ZERO (crazy2add(ONE(NIL),crazy2add(c1,c2)))
  | (MONE c1,MONE c2) -> ZERO (crazy2add(MONE(NIL),crazy2add(c1,c2)))
