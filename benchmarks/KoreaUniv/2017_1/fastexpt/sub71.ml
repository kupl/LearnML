(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n ->
  if n = 0 then 1
  else if n < 0 then raise (Failure ("ValueError: n must be a positive integer."))
  else if (n mod 2) <> 0 then b * (fastexpt b (n-1))
       else let re = (fastexpt b (n/2)) in re * re
