type crazy2=
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2add ((cz1: crazy2), (cz2: crazy2)): crazy2=
  match (cz1, cz2) with
  | (NIL, _ ) -> cz2
  | (_ , NIL) -> cz1
  | ((ZERO cz10), (ZERO cz20)) -> (ZERO (crazy2add (cz10, cz20)))
  | ((ONE cz10), (MONE cz20)) 
  | ((MONE cz10), (ONE cz20)) -> (ZERO (crazy2add (cz10, cz20)))
  | ((ONE cz10), (ZERO cz20)) 
  | ((ZERO cz10), (ONE cz20)) -> (ONE (crazy2add (cz10, cz20)))
  | ((MONE cz10), (ZERO cz20)) 
  | ((ZERO cz10), (MONE cz20)) -> (MONE (crazy2add (cz10, cz20)))
  | ((ONE cz10), (ONE cz20)) -> (ZERO (crazy2add ( (crazy2add (cz10,(ONE NIL))), cz20 ) ))
  | ((MONE cz10), (MONE cz20)) -> (ZERO (crazy2add ( (crazy2add (cz10,(MONE NIL))), cz20 ) ))
