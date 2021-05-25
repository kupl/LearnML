type crazy2 = NIL
            | ZERO of crazy2
            | ONE of crazy2
            | MONE of crazy2

let rec crazy2add (l, r) =
  match l with
  | NIL -> r
  | ZERO l_ -> (match r with
                | NIL -> l
                | ZERO r_ -> ZERO (crazy2add (l_, r_))
                | ONE r_ -> ONE (crazy2add (l_, r_))
                | MONE r_ -> MONE (crazy2add (l_, r_)))
  | ONE l_ -> (match r with
               | NIL -> l
               | ZERO r_ -> ONE (crazy2add (l_, r_))
               | ONE r_ -> ZERO (crazy2add ((crazy2add (l_, r_)), ONE NIL))
               | MONE r_ -> ZERO (crazy2add (l_, r_)))
  | MONE l_ -> (match r with
                | NIL -> l
                | ZERO r_ -> MONE (crazy2add (l_, r_))
                | ONE r_ -> ZERO (crazy2add (l_, r_))
                | MONE r_ -> ZERO (crazy2add ((crazy2add (l_, r_)), MONE NIL)))
