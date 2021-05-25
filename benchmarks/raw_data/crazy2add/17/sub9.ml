type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
type carry = NONE | POS | NEG

let crazy2add((z1: crazy2), (z2: crazy2)) =
let rec crazy2adder((x: crazy2), (y: crazy2), (carry: carry)) =
  match carry with
  | NONE -> ( match (x, y) with
            | (NIL, _) -> y
            | (ZERO(c1), ZERO(c2)) -> ZERO(crazy2adder(c1, c2, NONE))
            | (ZERO(c1), ONE(c2)) -> ONE(crazy2adder(c1, c2, NONE))
            | (ZERO(c1), MONE(c2)) -> MONE(crazy2adder(c1, c2, NONE))
            | (ONE(c1), ONE(c2)) -> ZERO(crazy2adder(c1, c2, POS))
            | (ONE(c1), MONE(c2)) -> ZERO(crazy2adder(c1, c2, NONE))
            | (MONE(c1), MONE(c2)) -> ZERO(crazy2adder(c1, c2, NEG))
            | (_, _) -> crazy2adder(y, x, carry)
            )
  | POS -> ( match (x, y) with
            | (NIL, _) -> (match y with
              | NIL -> ONE(NIL)
              | MONE(c1) -> ZERO(c1)
              | ZERO(c1) -> ONE(c1)
              | ONE(c1) -> ZERO(crazy2adder(c1, NIL, POS))
              )
            | (ZERO(c1), ZERO(c2)) -> ONE(crazy2adder(c1, c2, NONE))
            | (ZERO(c1), ONE(c2)) -> ZERO(crazy2adder(c1, c2, POS))
            | (ZERO(c1), MONE(c2)) -> ZERO(crazy2adder(c1, c2, NONE))
            | (ONE(c1), ONE(c2)) -> ONE(crazy2adder(c1, c2, POS))
            | (ONE(c1), MONE(c2)) -> ONE(crazy2adder(c1, c2, NONE))
            | (MONE(c1), MONE(c2)) -> MONE(crazy2adder(c1, c2, NONE))
            | (_, _) -> crazy2adder(y, x, carry)
            )
  | NEG -> ( match (x, y) with
            | (NIL, _) -> (match y with
              | NIL -> MONE(NIL)
              | MONE(c1) -> ZERO(crazy2adder(c1, NIL, NEG))
              | ZERO(c1) -> MONE(c1)
              | ONE(c1) -> ZERO(c1)
              )
            | (ZERO(c1), ZERO(c2)) -> MONE(crazy2adder(c1, c2, NONE))
            | (ZERO(c1), ONE(c2)) -> ZERO(crazy2adder(c1, c2, NONE))
            | (ZERO(c1), MONE(c2)) -> ZERO(crazy2adder(c1, c2, NEG))
            | (ONE(c1), ONE(c2)) -> ONE(crazy2adder(c1, c2, NONE))
            | (ONE(c1), MONE(c2)) -> MONE(crazy2adder(c1, c2, NONE))
            | (MONE(c1), MONE(c2)) -> MONE(crazy2adder(c1, c2, NEG))
            | (_, _) -> crazy2adder(y, x, carry)
            )
  in crazy2adder(z1, z2, NONE)
