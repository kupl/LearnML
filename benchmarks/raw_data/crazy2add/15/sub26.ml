type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2add (c1, c2) =
  let rec crazy2addsub c1 c2 carry =
  match c1, c2, carry with
  | NIL, _, 0 -> c2
  | NIL, _, 1 -> crazy2addsub (ONE NIL) c2 0
  | NIL, _, (-1) -> crazy2addsub (MONE NIL) c2 0
  | _, NIL, 0 -> c1
  | _, NIL, 1 -> crazy2addsub (ONE NIL) c1 0
  | _, NIL, (-1) -> crazy2addsub (MONE NIL) c1 0
  | ZERO cc1, ZERO cc2, 0 -> ZERO (crazy2addsub cc1 cc2 0)
  | ZERO cc1, ZERO cc2, 1 -> ONE (crazy2addsub cc1 cc2 0)
  | ZERO cc1, ZERO cc2, (-1) -> MONE (crazy2addsub cc1 cc2 0)
  | ZERO cc1, ONE cc2, 0 -> ONE (crazy2addsub cc1 cc2 0)
  | ZERO cc1, ONE cc2, 1 -> ZERO (crazy2addsub cc1 cc2 1)
  | ZERO cc1, ONE cc2, (-1) -> ZERO (crazy2addsub cc1 cc2 0)
  | ZERO cc1, MONE cc2, 0 -> MONE (crazy2addsub cc1 cc2 0)
  | ZERO cc1, MONE cc2, 1 -> ZERO (crazy2addsub cc1 cc2 0)
  | ZERO cc1, MONE cc2, (-1) -> ZERO (crazy2addsub cc1 cc2 (-1))
  | ONE cc1, ZERO cc2, 0 -> ONE (crazy2addsub cc1 cc2 0)
  | ONE cc1, ZERO cc2, 1 -> ZERO (crazy2addsub cc1 cc2 1)
  | ONE cc1, ZERO cc2, (-1) -> ZERO (crazy2addsub cc1 cc2 0)
  | ONE cc1, ONE cc2, 0 -> ZERO (crazy2addsub cc1 cc2 1)
  | ONE cc1, ONE cc2, 1 -> ONE (crazy2addsub cc1 cc2 1)
  | ONE cc1, ONE cc2, (-1) -> ONE (crazy2addsub cc1 cc2 0)
  | ONE cc1, MONE cc2, 0 -> ZERO (crazy2addsub cc1 cc2 0)
  | ONE cc1, MONE cc2, 1 -> ONE (crazy2addsub cc1 cc2 0)
  | ONE cc1, MONE cc2, (-1) -> MONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, ZERO cc2, 0 -> MONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, ZERO cc2, 1 -> ZERO (crazy2addsub cc1 cc2 0)
  | MONE cc1, ZERO cc2, (-1) -> ZERO (crazy2addsub cc1 cc2 (-1))
  | MONE cc1, ONE cc2, 0 -> ZERO (crazy2addsub cc1 cc2 0)
  | MONE cc1, ONE cc2, 1 -> ONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, ONE cc2, (-1) -> MONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, MONE cc2, 0 -> ZERO (crazy2addsub cc1 cc2 (-1))
  | MONE cc1, MONE cc2, 1 -> MONE (crazy2addsub cc1 cc2 0)
  | MONE cc1, MONE cc2, (-1) -> MONE (crazy2addsub cc1 cc2 (-1))
  | _ -> NIL
  in
  crazy2addsub c1 c2 0
