(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 6
 *)

type crazy2 = NIL 
	      | ZERO of crazy2
	      | ONE of crazy2
	      | MONE of crazy2;;

exception Error of string;; 

let crazy2val x = 
  let rec myc2v x= match x with
      NIL -> 0
    | ZERO (y) -> 0 + 2 * (myc2v y)
    | ONE (y) -> 1 + 2 * (myc2v y)
    | MONE (y) -> -1 + 2 * (myc2v y)
  in
    if x=NIL then raise (Error("NIL is not a number."))
    else
      myc2v x;;
