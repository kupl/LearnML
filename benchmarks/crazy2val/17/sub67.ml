type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val (x: crazy2): int = 
  match x with
  | NIL -> 0
  | ZERO a -> 2*(crazy2val a)
  | ONE a -> int_of_float (2.**0.) + 2*(crazy2val a)
  | MONE a -> (-1) * int_of_float (2.**0.) + 2*(crazy2val a)
