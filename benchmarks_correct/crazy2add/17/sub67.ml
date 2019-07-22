type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2


let rec crazy2add (crz1, crz2) =
  match crz1 with
  | NIL -> crz2
  | ZERO a ->
      (match crz2 with
      | NIL -> crz1
      | ZERO b -> ZERO(crazy2add(a, b))
      | ONE b -> ONE(crazy2add(a,b))
      | MONE b -> MONE(crazy2add(a,b)))
  | ONE a ->
      (match crz2 with
      | NIL -> crz1
      | ZERO b -> ONE(crazy2add(a, b))
      | ONE b -> ZERO(crazy2add((ONE NIL),(crazy2add(a,b))))
      | MONE b -> ZERO(crazy2add(a,b)))
  | MONE a ->
      (match crz2 with
      | NIL -> crz1
      | ZERO b -> MONE(crazy2add(a, b))
      | ONE b -> ZERO(crazy2add(a,b))
      | MONE b -> ZERO(crazy2add((MONE NIL), (crazy2add(a,b)))))


