type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

type crazy2digit = GZERO | GONE | GMONE | GDMONE | GDONE | GTMONE | GTONE;;

exception InconsistencyError
exception DigitAddError

let crazy2digit_add ((v1 : crazy2digit), (v2 : crazy2digit)) =
  match v1 with
    GDMONE ->
      (match v2 with GMONE -> GTMONE | GZERO -> GDMONE | GONE -> GMONE | _ -> GDMONE)
  | GMONE ->
      (match v2 with GMONE -> GDMONE | GZERO -> GMONE | GONE -> GZERO | _ -> GMONE)
  | GZERO ->
      (match v2 with GMONE -> GMONE | GZERO -> GZERO | GONE -> GONE | _ -> GZERO)
  | GONE ->
      (match v2 with GMONE -> GZERO | GZERO -> GONE | GONE -> GDONE | _ -> GONE)
  | GDONE ->
      (match v2 with GMONE -> GONE | GZERO -> GDONE | GONE -> GTONE | _ -> GDONE)
  | _ -> raise DigitAddError;;

let rec crazy2add ((v1 : crazy2), (v2 : crazy2)) =
  let digit_identifier (v : crazy2) : (crazy2digit * crazy2) =
    match v with
      NIL -> (GZERO, NIL)
    | ZERO c -> (GZERO, c)
    | ONE c -> (GONE, c)
    | MONE c -> (GMONE, c) in
  let rec add ((v1 : crazy2), (v2 : crazy2), (carry : crazy2digit)) : crazy2 =
    match (v1, v2, carry) with
      (NIL, NIL, GZERO) -> NIL
    | (NIL, NIL, v) ->
      if v = GONE then ONE NIL
      else if v = GZERO then ZERO NIL
      else MONE NIL
    | (_, _, _) ->
      let (d1, c1) = digit_identifier v1 in
      let (d2, c2) = digit_identifier v2 in
      let s_temp = crazy2digit_add(d1, d2) in
      let s = crazy2digit_add(s_temp, carry) in
      (match s with
        GTMONE -> MONE(add(c1, c2, GMONE))
      | GDMONE -> ZERO(add(c1, c2, GMONE))
      | GMONE -> MONE(add(c1, c2, GZERO))
      | GZERO -> ZERO(add(c1, c2, GZERO))
      | GONE -> ONE(add(c1, c2, GZERO))
      | GDONE -> ZERO(add(c1, c2, GONE))
      | GTONE -> ONE(add(c1, c2, GONE)))
  in
  add(v1, v2, GZERO);;
