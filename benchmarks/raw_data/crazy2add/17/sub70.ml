type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let crazy2add : crazy2 * crazy2 -> crazy2 = fun (a, b) ->
  let rec c2R : crazy2 * crazy2 * crazy2 -> crazy2 = fun(a, b, c) ->
  match a with
  | NIL -> (
    match b, c with
    | _, NIL -> NIL
    | NIL, ZERO y -> NIL
    | NIL, ONE y -> ONE(NIL)
    | NIL, MONE y -> MONE(NIL)
    | ZERO x, ZERO y -> ZERO (c2R(a, x, ZERO(NIL)))
    | ZERO x, ONE y -> ONE (c2R(a, x, ZERO(NIL)))
    | ZERO x, MONE y -> MONE (c2R(a, x, ZERO(NIL)))
    | ONE x, ZERO y -> ONE (c2R(a, x, ZERO(NIL)))
    | ONE x, ONE y -> ZERO (c2R(a, x, ONE(NIL)))
    | ONE x, MONE y -> ZERO (c2R(a, x, ZERO(NIL)))
    | MONE x, ZERO y -> MONE (c2R(a, x, ZERO(NIL)))
    | MONE x, ONE y -> ZERO (c2R(a, x, ZERO(NIL)))
    | MONE x, MONE y -> ZERO (c2R(a, x, MONE(NIL)))
    )
  | ZERO x -> (
    match b, c with
    | _, NIL -> NIL
    | NIL, ZERO z -> ZERO (c2R(x, NIL, ZERO(NIL)))
    | NIL, ONE z -> ONE (c2R(x, NIL, ZERO(NIL)))
    | NIL, MONE z -> MONE (c2R(x, NIL, ZERO(NIL)))
    | ZERO y, ZERO z -> ZERO (c2R(x, y, ZERO(NIL)))
    | ZERO y, ONE z -> ONE (c2R(x, y, ZERO(NIL)))
    | ZERO y, MONE z -> MONE (c2R(x, y, ZERO(NIL)))
    | ONE y, ZERO z -> ONE (c2R(x, y, ZERO(NIL)))
    | ONE y, ONE z -> ZERO (c2R(x, y, ONE(NIL)))
    | ONE y, MONE z -> ZERO (c2R(x, y, ZERO(NIL)))
    | MONE y, ZERO z -> MONE (c2R(x, y, ZERO(NIL)))
    | MONE y, ONE z -> ZERO (c2R(x, y, ZERO(NIL)))
    | MONE y, MONE z -> ZERO (c2R(x, y, MONE(NIL)))
    )
  | ONE x -> (
    match b, c with
    | _, NIL -> NIL
    | NIL, ZERO z -> ONE(c2R(x, NIL, ZERO(NIL)))
    | NIL, ONE z -> ZERO(c2R(x, NIL, ONE(NIL)))
    | NIL, MONE z -> ZERO(c2R(x, NIL, ZERO(NIL)))
    | ZERO y, ZERO z ->  ONE(c2R(x, y, ZERO(NIL)))
    | ZERO y, ONE z -> ZERO(c2R(x, y, ONE(NIL)))
    | ZERO y, MONE z -> ZERO(c2R(x, y, ZERO(NIL)))
    | ONE y, ZERO z -> ZERO (c2R(x, y, ONE(NIL)))
    | ONE y, ONE z -> ONE (c2R(x, y, ONE(NIL)))
    | ONE y, MONE z -> ONE (c2R(x, y, ZERO(NIL)))
    | MONE y, ZERO z -> ZERO (c2R(x, y, ZERO(NIL)))
    | MONE y, ONE z -> ONE (c2R(x, y, ZERO(NIL)))
    | MONE y, MONE z -> MONE (c2R(x, y, ZERO(NIL)))
    )
  | MONE x -> (
    match b, c with
    | _, NIL -> NIL
    | NIL, ZERO z -> MONE(c2R(x, NIL, ZERO(NIL)))
    | NIL, ONE z -> ZERO(c2R(x, NIL, ZERO(NIL)))
    | NIL, MONE z -> ZERO(c2R(x, NIL, MONE(NIL)))
    | ZERO y, ZERO z ->  MONE(c2R(x, y, ZERO(NIL)))
    | ZERO y, ONE z -> ZERO(c2R(x, y, ZERO(NIL)))
    | ZERO y, MONE z -> ZERO(c2R(x, y, MONE(NIL)))
    | ONE y, ZERO z -> ZERO (c2R(x, y, ZERO(NIL)))
    | ONE y, ONE z -> ONE (c2R(x, y, ZERO(NIL)))
    | ONE y, MONE z -> MONE (c2R(x, y, ZERO(NIL)))
    | MONE y, ZERO z -> ZERO (c2R(x, y, MONE(NIL)))
    | MONE y, ONE z -> MONE (c2R(x, y, ZERO(NIL)))
    | MONE y, MONE z -> MONE (c2R(x, y, MONE(NIL)))
    )
  in
  c2R(a, b, ZERO(NIL))
