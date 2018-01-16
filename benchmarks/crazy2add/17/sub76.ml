type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let crazy2add ((c1 : crazy2), (c2:crazy2)) : crazy2 =
  let zero = 0 in
  let one = 1 in
  let rec carryadder ((c1 : crazy2), (c2 : crazy2), (carry : int)) : crazy2 =
    match c1 with
    | NIL ->
    if (carry == one) then carryadder(ONE(NIL), c2, 0)
    else  if (carry == zero) then c2
    else carryadder(MONE(NIL), c2, 0)
    | ONE(d1) -> (match (c2) with
        | ONE(d2) -> 
          if (carry == one) then ONE(carryadder(d1, d2, 1))
          else  if (carry == zero) then ZERO(carryadder(d1, d2, 1))
          else ONE(carryadder(d1, d2, 0))

        | ZERO(d2) -> 
          if (carry == one) then ZERO(carryadder(d1, d2, 1))
          else  if (carry == zero) then ONE(carryadder(d1, d2, 0))
          else ZERO(carryadder(d1, d2, 0))

        | MONE(d2) -> 
          if (carry == one) then ONE(carryadder(d1, d2, 0))
          else if (carry == zero) then ZERO(carryadder(d1, d2, 0))
          else MONE(carryadder(d1, d2, 0))

        | NIL -> 
        if (carry == one) then carryadder(ONE(NIL), c1, 0)
        else  if (carry == zero) then c1
        else carryadder(MONE(NIL), c1, 0)
      )
    | ZERO(d1) -> (match (c2) with
        | ONE(d2) -> 
          if (carry == one) then ZERO(carryadder(d1, d2, 1))
          else  if (carry == zero) then ONE(carryadder(d1, d2, 0))
          else ZERO(carryadder(d1, d2, 0))

        | ZERO(d2) -> 
          if (carry == one) then ONE(carryadder(d1, d2, 0))
          else if (carry == zero) then ZERO(carryadder(d1, d2, 0))
          else MONE(carryadder(d1, d2, 0))

        | MONE(d2) -> 
          if (carry == one) then ZERO(carryadder(d1, d2, 0))
          else if (carry == zero) then MONE(carryadder(d1, d2, 0))
          else ZERO(carryadder(d1, d2, -1))

          | NIL -> 
          if (carry == one) then carryadder(ONE(NIL), c1, 0)
          else  if (carry == zero) then c1
          else carryadder(MONE(NIL), c1, 0)
      )
    | MONE(d1) -> (match (c2) with
        | ONE(d2) -> 
          if (carry == one) then ONE(carryadder(d1, d2, 0))
          else if (carry == zero) then ZERO(carryadder(d1, d2, 0))
          else MONE(carryadder(d1, d2, 0))

        | ZERO(d2) -> 
          if (carry == one) then ZERO(carryadder(d1, d2, 0))
          else if (carry == zero) then MONE(carryadder(d1, d2, 0))
          else ZERO(carryadder(d1, d2, -1))

        | MONE(d2) -> 
          if (carry == one) then MONE(carryadder(d1, d2, 0))
          else if (carry == zero) then ZERO(carryadder(d1, d2, -1))
          else MONE(carryadder(d1, d2, -1))

          | NIL -> 
          if (carry == one) then carryadder(ONE(NIL), c1, 0)
          else  if (carry == zero) then c1
          else carryadder(MONE(NIL), c1, 0)
      )
  in
  carryadder(c2, c1, 0)