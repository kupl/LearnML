type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2

let rec crazy2val : crazy2 -> int = fun a ->
   match a with
  | NIL -> 0
  | ZERO b -> 2*(crazy2val b)
  | ONE b -> 1 + 2*(crazy2val b)
  | MONE b -> -1 + 2*(crazy2val b)

let rec crazy2add : crazy2 * crazy2 -> crazy2 = fun(a,b) ->
   match a with
   | NIL -> (match b with 
             | NIL -> NIL
             | ONE c -> ONE(crazy2add(a,c))
             | MONE c -> MONE(crazy2add(a,c))
             | ZERO c -> ZERO(crazy2add(a,c))
             )
   | ONE d -> (match b with 
             | NIL -> ONE(crazy2add(d,b))
             | ONE c -> crazy2add(ZERO(crazy2add(d,c)),ZERO(ONE NIL))
             | MONE c -> ZERO(crazy2add(d,c))
             | ZERO c -> ONE(crazy2add(d,c) )
             )
   | MONE d -> (match b with 
             | NIL -> MONE(crazy2add(d,b))
             | ONE c -> ZERO(crazy2add(d,c))
             | MONE c -> crazy2add(ZERO(crazy2add(d,c)),ZERO(MONE NIL))
             | ZERO c -> MONE(crazy2add(d,c))
             )
   | ZERO d -> (match b with 
             | NIL -> ZERO(crazy2add(d,b))
             | ONE c -> ONE(crazy2add(d,c))
             | MONE c -> MONE(crazy2add(d,c))
             | ZERO c -> ZERO(crazy2add(d,c))
             )

