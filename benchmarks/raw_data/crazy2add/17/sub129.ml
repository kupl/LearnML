type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (x , y) ->
  let rec plus : crazy2 -> crazy2 = fun x ->
    match x with
    | NIL -> ONE NIL
    | ZERO x1 -> ONE x1
    | MONE x1 -> ZERO x1
    | ONE x1 -> ZERO (plus x1) in
  let rec minus : crazy2 -> crazy2 = fun x ->
    match x with
    | NIL -> MONE NIL
    | ZERO x1 -> MONE x1
    | MONE x1 -> ZERO (minus x1)
    | ONE x1 -> ZERO x1 in
  match (x , y) with
  | (NIL, y ) -> y
  | (x , NIL) -> x
  | (ZERO x1, ZERO y1) -> ZERO (crazy2add (x1 , y1))
  | (ZERO x1, ONE y1) -> ONE (crazy2add (x1, y1))
  | (ZERO x1, MONE y1) -> MONE (crazy2add (x1, y1))
  | (ONE x1, ZERO y1) -> ONE (crazy2add (x1, y1))
  | (ONE x1, MONE y1) -> ZERO (crazy2add (x1, y1))
  | (ONE x1, ONE y1) -> ZERO (crazy2add (plus x1, y1))
  | (MONE x1, MONE y1) -> ZERO (crazy2add (minus x1, y1))
  | (MONE x1, ZERO y1) -> MONE (crazy2add (x1, y1))
  | (MONE x1, ONE y1) -> ZERO (crazy2add (x1, y1))