type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2add (pair : crazy2 * crazy2) : crazy2 =
  let (l,r) = pair in
  match (l,r) with
  |(ZERO(n),r) ->
    (match r with
    |NIL -> ZERO(n)
    |ONE(m) -> ONE(crazy2add(n,m))
    |ZERO(m) -> ZERO(crazy2add(n,m))
    |MONE(m) -> MONE(crazy2add(n,m)))
  |(ONE(n),r) ->
    (match r with
    |NIL -> ONE(n)
    |ZERO(m) -> ONE(crazy2add(n,m))
    |ONE(m) -> ZERO(crazy2add(crazy2add(n,m),ONE(NIL)))
    |MONE(m) -> ZERO(crazy2add(n,m)))
  |(MONE(n),r) ->
    (match r with
    |NIL -> MONE(n)
    |ONE(m) -> ZERO(crazy2add(n,m))
    |ZERO(m) -> MONE(crazy2add(n,m))
    |MONE(m) -> ZERO(crazy2add(crazy2add(n,m),MONE(NIL))))
  |(l,NIL) -> l
  |(NIL,r) -> r


