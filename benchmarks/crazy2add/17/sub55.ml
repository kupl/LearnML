type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2val2 : crazy2 * int -> int = fun (a,b) ->
  match a with
  | NIL -> 0
  | ZERO(c) -> crazy2val2 (c,2*b)
  | ONE(c) -> b + crazy2val2 (c,2*b)
  | MONE(c) -> -b + crazy2val2 (c,2*b)

let crazy2val : crazy2 -> int = fun a ->
  crazy2val2 (a,1)

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (a,b) ->
  match a with
  | NIL -> b
  | ZERO(c) -> (
    match b with
    | NIL -> a
    | ZERO(d) -> ZERO (crazy2add (c, d))
    | ONE(d) -> ONE (crazy2add (c, d))
    | MONE(d) -> MONE (crazy2add (c,d))
  )
  | ONE(c) -> (
    match b with
    | NIL -> a
    | ZERO(d) -> ONE (crazy2add (c,d))
    | ONE(d) -> ZERO(crazy2add(crazy2add(c,d),ONE(NIL)))
    | MONE(d) -> ZERO(crazy2add(c,d))
  )
  | MONE(c) -> (
    match b with
    | NIL -> a
    | ZERO(d) -> MONE(crazy2add(c,d))
    | ONE(d) -> ZERO(crazy2add(c,d))
    | MONE(d) -> ZERO(crazy2add(crazy2add(c,d),MONE(NIL)))
  )
