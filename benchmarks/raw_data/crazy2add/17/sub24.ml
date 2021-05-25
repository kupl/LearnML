type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

(* comment crazy2val out before handout *)
let rec crazy2add (c2_1, c2_2) =
  match c2_1 with
  | NIL -> c2_2
  | ZERO c2_1_pre -> (
      match c2_2 with
      | NIL -> c2_1
      | ZERO c2_2_pre -> ZERO (crazy2add(c2_1_pre, c2_2_pre))
      | ONE c2_2_pre -> ONE (crazy2add(c2_1_pre, c2_2_pre))
      | MONE c2_2_pre -> MONE (crazy2add(c2_1_pre, c2_2_pre))
    )
  | ONE c2_1_pre -> (
      match c2_2 with
      | NIL -> c2_1
      | ZERO c2_2_pre -> ONE (crazy2add(c2_1_pre, c2_2_pre))
      | ONE c2_2_pre -> ZERO (crazy2add(crazy2add(c2_1_pre, ONE NIL), c2_2_pre))
      | MONE c2_2_pre -> ZERO (crazy2add(c2_1_pre, c2_2_pre))
    )
  | MONE c2_1_pre -> (
      match c2_2 with
      | NIL -> c2_1
      | ZERO c2_2_pre -> MONE (crazy2add(c2_1_pre, c2_2_pre))
      | ONE c2_2_pre -> ZERO (crazy2add(c2_1_pre, c2_2_pre))
      | MONE c2_2_pre -> ZERO (crazy2add(crazy2add(c2_1_pre, MONE NIL), c2_2_pre))
    )
