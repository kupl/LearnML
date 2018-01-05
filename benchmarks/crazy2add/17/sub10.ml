(* Homework 2 - Exercise 3
 * 2011-10492 Jaeyeong Yang *)
type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun (x, y) ->
  let trimmed_zero: crazy2 -> crazy2 = fun c ->
    match c with
    | NIL -> NIL
    | _ -> ZERO c
  in
  match (x, y) with
  | (NIL, NIL) -> NIL
  | (NIL, y) -> y
  | (x, NIL) -> x
  | (ZERO xl, ZERO yl) -> trimmed_zero (crazy2add (xl, yl))
  | (ONE xl, MONE yl) -> trimmed_zero (crazy2add (xl, yl))
  | (MONE xl, ONE yl) -> trimmed_zero (crazy2add (xl, yl))
  | (ZERO xl, ONE yl) -> ONE (crazy2add (xl, yl))
  | (ONE xl, ZERO yl) -> ONE (crazy2add (xl, yl))
  | (ZERO xl, MONE yl) -> MONE (crazy2add (xl, yl))
  | (MONE xl, ZERO yl) -> MONE (crazy2add (xl, yl))
  | (MONE xl, MONE yl) ->
    trimmed_zero (crazy2add (crazy2add (xl, yl), MONE NIL))
  | (ONE xl, ONE yl) ->
    trimmed_zero (crazy2add (crazy2add (xl, yl), ONE NIL))
