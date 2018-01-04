(* HW2 Exercise 3 sum of k crazy numbers *)

type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (num1_adding, num2_adding) ->
  match (num1_adding, num2_adding) with
  | (NIL, _) -> num2_adding
  | (_, NIL) -> num1_adding

  | (ZERO num1, ZERO num2) -> ZERO (crazy2add (num1, num2))
  | (ZERO num1, ONE num2) -> ONE (crazy2add (num1, num2))
  | (ZERO num1, MONE num2) -> MONE (crazy2add (num1, num2))

  | (ONE num1, ZERO num2) -> ONE (crazy2add (num1, num2))
  | (ONE num1, ONE num2) -> ZERO (crazy2add ((crazy2add (num1, num2)), ONE NIL))
  | (ONE num1, MONE num2) -> ZERO (crazy2add (num1, num2))

  | (MONE num1, ZERO num2) -> MONE (crazy2add (num1, num2))
  | (MONE num1, ONE num2) -> ZERO (crazy2add (num1, num2))
  | (MONE num1, MONE num2) -> ZERO (crazy2add ((crazy2add (num1, num2)), MONE NIL))

(* let _ = print_int(crazy2val (crazy2add (MONE (ONE (ONE NIL)), MONE (ONE NIL)))) *)
