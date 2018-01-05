(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 7
 *)

type crazy2 = NIL 
	      | ZERO of crazy2
	      | ONE of crazy2
	      | MONE of crazy2;;

exception Error of string;;

let crazy2add a b = 
  let rec myadd a b =
    match a with
	NIL -> b
      | ZERO(x) -> 
	  (match b with
	       NIL -> ZERO(x)
	     | ZERO(y) -> ZERO(myadd x y)
	     | ONE(y) -> ONE(myadd x y)
	     | MONE(y) -> MONE(myadd x y))
      | ONE(x) ->
	  (match b with
	       NIL -> ONE(x)
	     | ZERO(y) -> ONE(myadd x y)
	 | ONE(y) -> ZERO(myadd (ONE(NIL)) (myadd x y))
	 | MONE(y) -> ZERO(myadd x y))
      | MONE(x) ->
	  (match b with
	       NIL -> MONE(x)
	     | ZERO(y) -> MONE(myadd x y)
	     | ONE(y) -> ZERO(myadd x y)
	     | MONE(y) -> ZERO(myadd (MONE(NIL)) (myadd x y)))
  in
  let zz2z a =
    match a with 
	ZERO(ZERO(NIL)) -> ZERO(NIL)
      | _ -> a
  in
  let z2n a =
    match a with
	ZERO(NIL) -> NIL
      | _ -> a
  in
  let rec delZERO a =
    match a with 
	ZERO(x) -> zz2z (ZERO(delZERO x))
      | ONE (x) -> ONE (z2n(delZERO x))
      | MONE (x) -> MONE (z2n(delZERO x))
      | _ -> a
  in
    if a=NIL then raise (Error ("arg0 is NIL"))
    else if b=NIL then raise (Error ("arg1 is NIL"))
    else delZERO(myadd a b);;
	 
