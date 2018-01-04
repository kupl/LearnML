(*
  CSE/2015-21233/김종권
  Homework 2-2
*)

type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2


let rec crazy2add' (c1, c2) =
  match c1, c2 with
  | NIL, NIL -> NIL
  | NIL, ZERO z -> ZERO (crazy2add' (NIL, z) )
  | NIL, ONE o -> ONE (crazy2add' (NIL, o) )
  | NIL, MONE m -> MONE (crazy2add' (NIL, m) )
                     
  | ZERO z, NIL -> ZERO (crazy2add' (z, NIL) )
  | ZERO z1, ZERO z2 -> ZERO (crazy2add' (z1, z2) )
  | ZERO z, ONE o -> ONE (crazy2add' (z, o) )
  | ZERO z, MONE m -> MONE (crazy2add' (z, m) )
                        
  | ONE o, NIL -> ONE (crazy2add' (o, NIL) )
  | ONE o, ZERO z -> ONE (crazy2add' (o, z) )
  | ONE o1, ONE o2 ->
    ZERO (crazy2add' (ONE(NIL), (crazy2add' (o1, o2) )))
  | ONE o, MONE m -> ZERO (crazy2add' (o, m) )

  | MONE m, NIL -> MONE (crazy2add' (m, NIL) )
  | MONE m, ZERO z -> MONE (crazy2add' (m, z) )
  | MONE m, ONE o -> ZERO (crazy2add' (m, o) )
  | MONE m1, MONE m2 ->
    ZERO (crazy2add' (MONE(NIL),(crazy2add' (m1, m2) )))
                          
let crazy2add (c1, c2) =
  crazy2add' (c1, c2)
