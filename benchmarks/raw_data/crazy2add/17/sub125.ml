
type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2


type plusminus = NONE | PLUS | MINUS

let rec add1 c =
  match c with
    | NIL -> ONE (NIL)
    | ZERO c_ -> ONE c_
    | ONE c_ -> ZERO (add1 c_)
    | MONE c_ -> ZERO c_
and minus1 c =
  match c with
    | NIL -> MONE (NIL)
    | ZERO c_ -> MONE c_
    | ONE c_ -> ZERO c_
    | MONE c_ -> ZERO (minus1 c_)

let crazy2add : crazy2 * crazy2 -> crazy2 = fun (c0, c1) ->
  let rec crazy2add2 c0 c1 pm =
    match pm with
      | NONE -> (
          match (c0, c1) with
            | (NIL, _) -> c1
            | (_, NIL) -> c0
            | (ZERO c0_, ZERO c1_) | (ONE c0_, MONE c1_) | (MONE c0_, ONE c1_) ->
                ZERO (crazy2add2 c0_ c1_ NONE)
            | (ZERO c0_, ONE c1_) | (ONE c0_, ZERO c1_) ->
                ONE (crazy2add2 c0_ c1_ NONE)
            | (ZERO c0_, MONE c1_) | (MONE c0_, ZERO c1_) ->
                MONE (crazy2add2 c0_ c1_ NONE)
            | (ONE c0_, ONE c1_) ->
                ZERO (crazy2add2 c0_ c1_ PLUS)
            | (MONE c0_, MONE c1_) ->
                ZERO (crazy2add2 c0_ c1_ MINUS)
        )
      | PLUS -> (
          match (c0, c1) with
            | (NIL, _) -> add1 c1
            | (_, NIL) -> add1 c0
            | (ZERO c0_, ZERO c1_) | (ONE c0_, MONE c1_) | (MONE c0_, ONE c1_) ->
                ONE (crazy2add2 c0_ c1_ NONE)
            | (ZERO c0_, ONE c1_) | (ONE c0_, ZERO c1_) ->
                ZERO (crazy2add2 c0_ c1_ PLUS)
            | (ZERO c0_, MONE c1_) | (MONE c0_, ZERO c1_) ->
                ZERO (crazy2add2 c0_ c1_ NONE)
            | (ONE c0_, ONE c1_) ->
                ONE (crazy2add2 c0_ c1_ PLUS)
            | (MONE c0_, MONE c1_) ->
                MONE (crazy2add2 c0_ c1_ NONE)
        )
      | MINUS -> (
          match (c0, c1) with
            | (NIL, _) -> minus1 c1
            | (_, NIL) -> minus1 c0
            | (ZERO c0_, ZERO c1_) | (ONE c0_, MONE c1_) | (MONE c0_, ONE c1_) ->
                MONE (crazy2add2 c0_ c1_ NONE)
            | (ZERO c0_, ONE c1_) | (ONE c0_, ZERO c1_) ->
                ZERO (crazy2add2 c0_ c1_ NONE)
            | (ZERO c0_, MONE c1_) | (MONE c0_, ZERO c1_) ->
                ZERO (crazy2add2 c0_ c1_ MINUS)
            | (ONE c0_, ONE c1_) ->
                ONE (crazy2add2 c0_ c1_ NONE)
            | (MONE c0_, MONE c1_) ->
                MONE (crazy2add2 c0_ c1_ MINUS)
        )
  in
    crazy2add2 c0 c1 NONE
