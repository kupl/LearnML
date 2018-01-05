(* 2015-11380 박찬양 HW2_2 *)

type crazy2 = NIL 
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val: crazy2 -> int = fun cra2 ->
  match cra2 with 
  | NIL -> 0
  | ZERO a -> (2 * crazy2val a)
  | ONE a -> (1 + 2 * crazy2val a)
  | MONE a -> (-1 + 2 * crazy2val a)

 