type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2
(*
let rec crazy2val (x: crazy2): int = 
  match x with
  | NIL -> 0
  | ZERO (y) -> (2 * crazy2val (y))
  | ONE (z) -> (1 + 2 * crazy2val (z))
  | MONE (w) -> (-1 + 2 * crazy2val (w))
*)

let rec crazy2add ((x: crazy2), (y: crazy2)): crazy2 =
  match x, y with
  | NIL, _ -> y
  | _, NIL -> x 
  | ZERO a, ZERO b | MONE a, ONE b | ONE a, MONE b -> ZERO (crazy2add (a, b))
  | ONE a, ZERO b | ZERO a, ONE b -> ONE (crazy2add (a, b))
  | MONE a, ZERO b | ZERO a, MONE b -> MONE (crazy2add (a, b))
  | ONE a, ONE b -> ZERO (crazy2add (ONE (NIL), crazy2add (a, b)))
  | MONE a, MONE b -> ZERO (crazy2add (MONE (NIL), crazy2add (a, b)))
  