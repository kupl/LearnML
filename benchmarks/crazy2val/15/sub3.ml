(*
    PL 2-1
    2008-11609 박성원
*)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val c =
  match c with
  | NIL -> 0
  | ZERO d -> (crazy2val d) * 2
  | ONE d -> (crazy2val d) * 2 + 1
  | MONE d -> (crazy2val d) * 2 - 1
