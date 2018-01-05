(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 6
 *)

type crazy2 = NIL 
	      | ZERO of crazy2
	      | ONE of crazy2
	      | MONE of crazy2;;

let rec crazy2val x = match x with
    NIL -> 0
  | ZERO (y) -> 0 + 2 * (crazy2val y)
  | ONE (y) -> 1 + 2 * (crazy2val y)
  | MONE (y) -> -1 + 2 * (crazy2val y);;
