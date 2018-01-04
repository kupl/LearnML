type crazy2 = NIL 
  | ZERO of crazy2 
  | ONE of crazy2 
  | MONE of crazy2
let rec crazy2val: crazy2 -> int = function
  | NIL -> 0
  | ZERO(a) -> 2 * crazy2val(a)
  | ONE(a) -> 2 * crazy2val(a) + 1
  | MONE(a) -> 2 * crazy2val(a) - 1
let rec crazy2add: crazy2 * crazy2 -> crazy2 = function
  (a,b) -> 
    (match (a,b) with
      |NIL, _ -> b
      |_, NIL -> a
      |(ZERO(_a), ZERO(_b)) -> ZERO(crazy2add(_a,_b))
      |(ZERO(_a), ONE(_b)) -> ONE(crazy2add(_a,_b))
      |(ZERO(_a), MONE(_b)) -> MONE(crazy2add(_a,_b))
      |(ONE(_a), ZERO(_b)) -> ONE(crazy2add(_a,_b))
      |(ONE(_a), ONE(_b)) -> ZERO(crazy2add(crazy2add(_a,_b), ONE(NIL)))
      |(ONE(_a), MONE(_b)) -> ZERO(crazy2add(_a,_b))
      |(MONE(_a), ZERO(_b)) -> MONE(crazy2add(_a,_b))
      |(MONE(_a), ONE(_b)) -> ZERO(crazy2add(_a,_b))
      |(MONE(_a), MONE(_b)) -> ZERO(crazy2add(crazy2add(_a,_b), MONE(NIL)))
    )
    

   