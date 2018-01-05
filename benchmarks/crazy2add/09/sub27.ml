(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 7
 *)

type crazy2 = NIL 
	      | ZERO of crazy2
	      | ONE of crazy2
	      | MONE of crazy2;;

let rec crazy2add a b = match a with
    NIL -> b
  | ZERO(x) -> 
      (match b with
	   NIL -> ZERO(x)
	 | ZERO(y) -> ZERO(crazy2add x y)
	 | ONE(y) -> ONE(crazy2add x y)
	 | MONE(y) -> MONE(crazy2add x y))
  | ONE(x) ->
      (match b with
	   NIL -> ONE(x)
	 | ZERO(y) -> ONE(crazy2add x y)
	 | ONE(y) -> ZERO(crazy2add (ONE(NIL)) (crazy2add x y))
	 | MONE(y) -> ZERO(crazy2add x y))
  | MONE(x) ->
      (match b with
	   NIL -> MONE(x)
	 | ZERO(y) -> MONE(crazy2add x y)
	 | ONE(y) -> ZERO(crazy2add x y)
	 | MONE(y) -> ZERO(crazy2add (MONE(NIL)) (crazy2add x y)));;
	 
