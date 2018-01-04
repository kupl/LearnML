type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

type carry = NONE | PLUS | MINUS

let crazy2add (a, b) =
  let carryToCrazy c subCrazy =
    match c with
      NONE -> ZERO subCrazy
    | PLUS -> ONE subCrazy
    | MINUS -> MONE subCrazy
  in
  let rec addWithCarry (a, b, c) =
    match (a, b, c) with
      (NIL, NIL, c) -> carryToCrazy c NIL
    | (NIL, b, c) -> addWithCarry (ZERO NIL, b, c)
    | (a, NIL, c) -> addWithCarry (a, ZERO NIL, c)
    | (ZERO subA, ZERO subB, c) ->
      carryToCrazy c (addWithCarry (subA, subB, NONE))
    | (ONE subA, ONE subB, c) ->
      carryToCrazy c (addWithCarry (subA, subB, PLUS))
    | (MONE subA, MONE subB, c) ->
      carryToCrazy c (addWithCarry (subA, subB, MINUS))
    | (ONE subA, MONE subB, c) | (MONE subA, ONE subB, c) ->
      carryToCrazy c (addWithCarry (subA, subB, NONE))
    | (ZERO subA, ONE subB, NONE) | (ONE subA, ZERO subB, NONE) ->
      ONE (addWithCarry (subA, subB, NONE))
    | (ZERO subA, MONE subB, NONE) | (MONE subA, ZERO subB, NONE) ->
      MONE (addWithCarry (subA, subB, NONE))
    | (ZERO subA, b, c) -> addWithCarry (carryToCrazy c subA, b, NONE)
    | (a, ZERO subB, c) -> addWithCarry (a, carryToCrazy c subB, NONE)
  in
  addWithCarry (a, b, NONE)
