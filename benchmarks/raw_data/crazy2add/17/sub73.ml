type crazy2 = NIL 
| ZERO of crazy2 
| ONE of crazy2 
| MONE of crazy2

let rec crazy2add (n , m) =
  match n with
  |NIL -> m
  |ZERO(n') -> (match m with
               |NIL -> NIL
               |ZERO(m') -> ZERO(crazy2add(n',m'))
               |ONE(m') -> ONE(crazy2add(n',m'))
               |MONE(m') -> MONE(crazy2add(n',m'))
               )
  |ONE(n') -> (match m with
              |NIL -> n
              |ZERO(m') -> ONE(crazy2add(n',m'))
              |ONE(m') -> ZERO(crazy2add(ONE(NIL),crazy2add(n',m')))
              |MONE(m') ->ZERO(crazy2add(n',m'))
              )

  |MONE(n') -> (match m with
              |NIL -> n
              |ZERO(m') -> MONE(crazy2add(n',m'))
              |ONE(m') -> ZERO(crazy2add(n',m'))
              |MONE(m') -> ZERO(crazy2add(MONE(NIL),crazy2add(n',m')))
  
              )