type crazy2=
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec crazy2val(cz:crazy2): int=
  match cz with
  | NIL -> 0
  | ZERO cz2 -> 2*(crazy2val cz2)
  | ONE cz2 -> 2*(crazy2val cz2)+1
  | MONE cz2 -> 2*(crazy2val cz2)-1

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

let _ = print_int (crazy2val (ONE (ZERO NIL))); print_endline("")
let _ = print_int (crazy2val (crazy2add ((ONE NIL), (ZERO NIL)))); print_endline("")

let _ = print_int (crazy2val (crazy2add ((ONE (ZERO NIL)), (ONE (ZERO NIL)))))

let _ = print_int (crazy2val (crazy2add (((ZERO (ONE (MONE NIL))), (ZERO (ONE (MONE NIL)))))))


