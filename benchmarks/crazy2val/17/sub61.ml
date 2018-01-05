(* 컴퓨터공학부 2013-11425 이창영 hw2_2 *)

type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val2 (c : crazy2) x : int =
  match c with
  | NIL -> 0
  | (ZERO a) -> 0 + crazy2val2 a (2*x)
  | (ONE a) -> (x*1) + crazy2val2 a (2*x)
  | (MONE a) -> (x*(-1)) + crazy2val2 a (2*x)

let crazy2val (c : crazy2) : int =
  crazy2val2 c 1
