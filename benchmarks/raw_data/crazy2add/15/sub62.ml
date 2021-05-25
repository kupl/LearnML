type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2addcarry : (int * (crazy2 * crazy2)) -> crazy2 = fun (x,y) ->
 match x with
 | 0 -> 
  (match y with
  | (NIL, NIL) -> NIL
  | (NIL, y2) -> y2
  | (y1, NIL) -> y1
  | ((ZERO y1_), (ZERO y2_)) -> ZERO (crazy2addcarry (0, (y1_, y2_)))
  | ((ZERO y1_), (ONE y2_)) -> ONE (crazy2addcarry (0, (y1_, y2_)))
  | ((ZERO y1_), (MONE y2_)) -> MONE (crazy2addcarry (0, (y1_, y2_)))
  | ((ONE y1_), (ZERO y2_)) -> ONE (crazy2addcarry (0, (y1_, y2_)))
  | ((ONE y1_), (ONE y2_)) -> ZERO (crazy2addcarry (1, (y1_, y2_)))
  | ((ONE y1_), (MONE y2_)) -> ZERO (crazy2addcarry (0, (y1_, y2_)))
  | ((MONE y1_), (ZERO y2_)) -> MONE (crazy2addcarry (0, (y1_, y2_)))
  | ((MONE y1_), (ONE y2_)) -> ZERO (crazy2addcarry (0, (y1_, y2_)))
  | ((MONE y1_), (MONE y2_)) -> ZERO (crazy2addcarry (-1, (y1_, y2_)))
  )
 | 1 -> 
  (match y with
  | (NIL, y2) -> crazy2addcarry (0, (ONE(NIL), y2))
  | (y1, NIL) -> crazy2addcarry (0, (y1, ONE(NIL)))
  | ((ZERO y1_), (ZERO y2_)) -> ONE (crazy2addcarry (0, (y1_, y2_)))
  | ((ZERO y1_), (ONE y2_)) -> ZERO (crazy2addcarry (1, (y1_, y2_)))
  | ((ZERO y1_), (MONE y2_)) -> ZERO (crazy2addcarry (0, (y1_, y2_)))
  | ((ONE y1_), (ZERO y2_)) -> ZERO (crazy2addcarry (1, (y1_, y2_)))
  | ((ONE y1_), (ONE y2_)) -> ONE (crazy2addcarry (1, (y1_, y2_)))
  | ((ONE y1_), (MONE y2_)) -> ONE (crazy2addcarry (0, (y1_, y2_)))
  | ((MONE y1_), (ZERO y2_)) -> ZERO (crazy2addcarry (0, (y1_, y2_)))
  | ((MONE y1_), (ONE y2_)) -> ONE (crazy2addcarry (0, (y1_, y2_)))
  | ((MONE y1_), (MONE y2_)) -> MONE (crazy2addcarry (0, (y1_, y2_)))
  )
 | -1 -> 
  (match y with
  | (NIL, y2) -> crazy2addcarry (0, (MONE(NIL), y2))
  | (y1, NIL) -> crazy2addcarry (0, (y1, MONE(NIL)))
  | ((ZERO y1_), (ZERO y2_)) -> MONE (crazy2addcarry (0, (y1_, y2_)))
  | ((ZERO y1_), (ONE y2_)) -> ZERO (crazy2addcarry (0, (y1_, y2_)))
  | ((ZERO y1_), (MONE y2_)) -> ZERO (crazy2addcarry (-1, (y1_, y2_)))
  | ((ONE y1_), (ZERO y2_)) -> ZERO (crazy2addcarry (0, (y1_, y2_)))
  | ((ONE y1_), (ONE y2_)) -> ONE (crazy2addcarry (0, (y1_, y2_)))
  | ((ONE y1_), (MONE y2_)) -> MONE (crazy2addcarry (0, (y1_, y2_)))
  | ((MONE y1_), (ZERO y2_)) -> ZERO (crazy2addcarry (-1, (y1_, y2_)))
  | ((MONE y1_), (ONE y2_)) -> MONE (crazy2addcarry (0, (y1_, y2_)))
  | ((MONE y1_), (MONE y2_)) -> MONE (crazy2addcarry (-1, (y1_, y2_)))
  )
 | n -> NIL
  
let crazy2add : (crazy2 * crazy2) -> crazy2 = fun (x,y) -> (crazy2addcarry (0, (x,y)))
