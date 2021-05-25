type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

exception InconsistencyError

let rec crazy2add ((v1 : crazy2), (v2 : crazy2)) =
  let digit_identifier (v : crazy2) : (int * crazy2) =
    match v with
      NIL -> (0, NIL)
    | ZERO c -> (0, c)
    | ONE c -> (1, c)
    | MONE c -> (-1, c) in
  let rec add ((v1 : crazy2), (v2 : crazy2), (carry : int)) : crazy2 =
    match (v1, v2, carry) with
      (NIL, NIL, 0) -> NIL
    | (NIL, NIL, v) ->
      if v = 1 then ONE NIL
      else if v = 0 then ZERO NIL
      else MONE NIL
    | (_, _, _) ->
      let (d1, c1) = digit_identifier v1 in
      let (d2, c2) = digit_identifier v2 in
      let s = d1 + d2 + carry in
      match s with
        -2 -> ZERO(add(c1, c2, -1))
      | -1 -> MONE(add(c1, c2, 0))
      |  0 -> ZERO(add(c1, c2, 0))
      | +1 -> ONE (add(c1, c2, 0))
      | +2 -> ZERO(add(c1, c2, 1))
      | _ -> raise InconsistencyError
  in
  add(v1, v2, 0);;
