type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val = (fun x ->
  match x with
  | NIL -> 0
  | ZERO a -> 0 + 2 * (crazy2val a)
  | ONE a -> 1 + 2 * (crazy2val a)
  | MONE a -> -1 + 2 * (crazy2val a)
  )

let rec crazy2add = (fun (x,y) ->
  match (x,y) with
  | (x, NIL) -> x
  | (NIL, y) -> y
  | (ZERO a, ZERO b) -> ZERO (crazy2add (a,b))
  | (ZERO a, ONE b) -> ONE (crazy2add (a,b))
  | (ZERO a, MONE b) -> MONE (crazy2add (a,b))
  | (ONE a, ZERO b) -> ONE (crazy2add (a,b))
  | (ONE a, ONE b) -> ZERO (crazy2add (ONE NIL, crazy2add (a,b)))
  | (ONE a, MONE b) -> ZERO (crazy2add (a,b))
  | (MONE a, ZERO b) -> MONE (crazy2add (a,b))
  | (MONE a, ONE b) -> ZERO (crazy2add (a,b))
  | (MONE a, MONE b) -> ZERO (crazy2add (MONE NIL, crazy2add (a,b)))
  )