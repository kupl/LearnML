exception RangeError
(*2011120109 Jung-su Han Homework*)

(* Problem 2 *)

let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
  if a>b then raise (RangeError)
  else if a=b then f a
  else (f a)+(sigma f (a+1) b);;

(* defines exception for range being large number to smaller number *)
