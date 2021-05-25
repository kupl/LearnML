type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2;;

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun (c1,c2) ->
  match c1 with
  | NIL -> c2
  | ZERO(c1in) ->
      (match c2 with
      | NIL -> c1
      | ZERO(c2in) -> ZERO((crazy2add (c1in, c2in)))
      | ONE(c2in) -> ONE((crazy2add (c1in, c2in)))
      | MONE(c2in) -> MONE((crazy2add (c1in, c2in))))
  | ONE(c1in) -> 
      (match c2 with
      | NIL -> c1
      | ZERO(c2in) -> ONE((crazy2add (c1in, c2in)))
      | ONE(c2in) -> ZERO((crazy2add ((crazy2add (c1in, c2in)), ONE(NIL))))
      | MONE(c2in) -> ZERO((crazy2add (c1in, c2in))))
  | MONE(c1in) ->
      (match c2 with
      | NIL -> c1
      | ZERO(c2in) -> MONE((crazy2add (c1in, c2in)))
      | ONE(c2in) -> ZERO((crazy2add (c1in, c2in)))
      | MONE(c2in) -> ZERO((crazy2add ((crazy2add (c1in, c2in)), MONE(NIL)))))
;;

