(* 컴퓨터공학부 2013-11425 이창영 hw2_3 *)

type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec up (c: crazy2) : crazy2 =
  match c with
  | NIL -> NIL
  | MONE a -> ZERO a
  | ZERO a -> ONE a
  | ONE a -> ZERO (up a)

let rec down (c: crazy2) : crazy2 =
  match c with
  | NIL -> NIL
  | ONE a -> ZERO a
  | ZERO a -> MONE a
  | MONE a -> ZERO (down a)

let rec crazy2add ((a : crazy2), (b : crazy2)) : crazy2 =
  match (a, b) with
  | (NIL, NIL) -> (ZERO NIL)
  | (_, NIL) -> a
  | (NIL, _) -> b
  | (ZERO c, ZERO d) -> ZERO (crazy2add (c, d))
  | (ZERO c, ONE d) -> ONE (crazy2add (c, d))
  | (ONE c, ZERO d) -> ONE (crazy2add (c, d))
  | (ZERO c, MONE d) -> MONE (crazy2add (c, d))
  | (MONE c, ZERO d) -> MONE (crazy2add (c, d))
  | (ONE c, ONE d) -> ZERO (up (crazy2add (c, d)))
  | (MONE c, MONE d) -> ZERO (down (crazy2add (c, d)))
  | (MONE c, ONE d) -> ZERO (crazy2add (c, d))
  | (ONE c, MONE d) -> ZERO (crazy2add (c, d))
