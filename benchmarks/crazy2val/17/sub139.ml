(* 컴퓨터공학과/2017-34165/김성국/2-2 *)
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2val x =
  match x with
  | NIL -> 0
  | ZERO xx -> 2*(crazy2val xx)
  | ONE xx -> 2*(crazy2val xx) + 1
  | MONE xx -> 2*(crazy2val xx) - 1
