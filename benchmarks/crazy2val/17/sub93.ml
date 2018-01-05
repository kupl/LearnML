(* HW2 Exercise 2 k crazy number *)

type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val: crazy2 -> int = fun num_evaluating ->
  match num_evaluating with
  | NIL -> 0
  | ZERO num -> (2 * (crazy2val num))
  | ONE num -> (1 + 2 * (crazy2val num))
  | MONE num -> (-1 + 2 * (crazy2val num))

(* let _ = print_int(crazy2val (ZERO(ONE(MONE NIL)))) *)
