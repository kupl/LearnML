(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 1
 *)

exception Error of string;;

let rec sigma f a b=
  if a>b then raise (Error("arg0 > arg1"))
  else if a=b then (f a)
  else (f a)+ (sigma f (a+1) b)
;;
 
