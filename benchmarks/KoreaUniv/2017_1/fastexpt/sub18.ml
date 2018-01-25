(* problem 1*)
let rec fastexpt : int -> int -> int
  = fun b n -> 
    if n = 0 then 1 
    else if n = 1 then b
    else let a = fastexpt b (n/2) in a*a*
                                     (if n mod 2 == 0 then 1 else b);;