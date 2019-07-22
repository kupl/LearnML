(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 2
 *)

exception Error of string;;

let rec iter (n, f) a = 
  if n=0 then a
  else if n<0 then raise (Error ("n is negative"))
  else f ((iter (n-1,f)) a);;
