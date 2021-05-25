type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
type carry = PLUS | MINUS | NONE

let crazy2add: crazy2 * crazy2 -> crazy2 = function (a, b) ->
  let rec fulladd: (crazy2*crazy2*carry) -> crazy2 = function (a, b, c) ->
    match (a, b, c) with
      | NIL, NIL, NONE -> ZERO(NIL)
      | NIL, NIL, PLUS -> ONE(NIL)
      | NIL, NIL, MINUS -> MONE(NIL)

      (* a == NIL *)
      | NIL, _, NONE -> b

      | NIL, MONE(b1), PLUS -> ZERO(b1)
      | NIL, ZERO(b1), PLUS -> ONE(b1)
      | NIL, ONE(b1), PLUS -> ZERO(fulladd(NIL, b1, PLUS))

      | NIL, ONE(b1), MINUS -> ZERO(b1)
      | NIL, ZERO(b1), MINUS -> MONE(b1)
      | NIL, MONE(b1), MINUS -> ZERO(fulladd(NIL, b1, MINUS))
      (* a == NIL *)

      (* b == NIL *)
      | _, NIL, NONE -> a

      | MONE(a1), NIL, PLUS -> ZERO(a1)
      | ZERO(a1), NIL, PLUS -> ONE(a1)
      | ONE(a1), NIL, PLUS -> ZERO(fulladd(a1, NIL, PLUS))

      | ONE(a1), NIL, MINUS -> ZERO(a1)
      | ZERO(a1), NIL, MINUS -> MONE(a1)
      | MONE(a1), NIL, MINUS -> ZERO(fulladd(NIL, a1, MINUS))
      (* b == NIL *)

      (* carry == NONE *)
      | ONE(a1), MONE(b1), NONE
      | MONE(a1), ONE(b1), NONE
      | ZERO(a1), ZERO(b1), NONE -> ZERO(fulladd(a1, b1, NONE))

      | ZERO(a1), ONE(b1), NONE
      | ONE(a1), ZERO(b1), NONE -> ONE(fulladd(a1, b1, NONE))

      | ZERO(a1), MONE(b1), NONE
      | MONE(a1), ZERO(b1), NONE -> MONE(fulladd(a1, b1, NONE))

      | ONE(a1), ONE(b1), NONE -> ZERO(fulladd(a1, b1, PLUS))
      | MONE(a1), MONE(b1), NONE -> ZERO(fulladd(a1, b1, MINUS))
      (* carry == ZERO *)

      (* carry == PLUS *)
      | MONE(a1), ZERO(b1), PLUS
      | ZERO(a1), MONE(b1), PLUS -> ZERO(fulladd(a1, b1, NONE))

      | ONE(a1), MONE(b1), PLUS
      | MONE(a1), ONE(b1), PLUS
      | ZERO(a1), ZERO(b1), PLUS -> ONE(fulladd(a1, b1, NONE))

      | MONE(a1), MONE(b1), PLUS -> MONE(fulladd(a1, b1, NONE))

      | ZERO(a1), ONE(b1), PLUS
      | ONE(a1), ZERO(b1), PLUS -> ZERO(fulladd(a1, b1, PLUS))

      | ONE(a1), ONE(b1), PLUS -> ONE(fulladd(a1, b1, PLUS))
      (* carry == PLUS *)

      (* carry == MINUS *)
      | ONE(a1), ZERO(b1), MINUS
      | ZERO(a1), ONE(b1), MINUS -> ZERO(fulladd(a1, b1, NONE))

      | ONE(a1), MONE(b1), MINUS
      | MONE(a1), ONE(b1), MINUS
      | ZERO(a1), ZERO(b1), MINUS -> MONE(fulladd(a1, b1, NONE))

      | ONE(a1), ONE(b1), MINUS -> ONE(fulladd(a1, b1, NONE))

      | ZERO(a1), MONE(b1), MINUS
      | MONE(a1), ZERO(b1), MINUS -> ZERO(fulladd(a1, b1, MINUS))

      | MONE(a1), MONE(b1), MINUS -> MONE(fulladd(a1, b1, MINUS))
      (* carry == MINUS *)

  in fulladd(a, b, NONE)
