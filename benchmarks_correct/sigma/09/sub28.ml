(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 1
 *)

let rec sigma f a b =
  if a>b then 0
  else (f a)+ (sigma f (a+1) b)
;;
 
