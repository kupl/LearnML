(*Problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> if (n mod 2=0) then (fastexpt b (n/2)) * (fastexpt b (n/2))
  else (if (n=1) then b else b*(fastexpt b (n-1)))
