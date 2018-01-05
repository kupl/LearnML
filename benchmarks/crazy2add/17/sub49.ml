type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (one, two) ->
  match (one, two) with 
  | (NIL, _) -> two
  | (_, NIL) -> one
  | (ZERO a, ZERO b) -> ZERO (crazy2add (a, b))
  | (ZERO a, ONE b) -> ONE (crazy2add (a, b))
  | (ZERO a, MONE b) -> MONE (crazy2add (a, b))
  | (ONE a, ZERO b) -> ONE (crazy2add (a, b))
  | (ONE a, ONE b) -> ZERO (crazy2add (ONE NIL, (crazy2add (a, b))))
  | (ONE a, MONE b) -> ZERO (crazy2add (a, b))
  | (MONE a, ZERO b) -> MONE (crazy2add (a, b))
  | (MONE a, ONE b) -> ZERO (crazy2add (a, b))
  | (MONE a, MONE b) -> ZERO (crazy2add (MONE NIL, (crazy2add (a, b))))

let rec print_c crazy = 
  match crazy with
  | NIL -> print_endline ""
  | ZERO a -> print_string("0");print_c a
  | ONE a -> print_string("+");print_c a
  | MONE a -> print_string("-");print_c a

let print_crazy (one, two) = print_c(crazy2add (one, two))

