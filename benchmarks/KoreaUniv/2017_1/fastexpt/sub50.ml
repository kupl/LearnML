(* problem 1*)
let is_even x = x mod 2 = 0;;

let rec fastexpt : int -> int -> int
= fun b n -> 
if n = 0 then 1
else if n = 2 then b * b
else if is_even n then (fastexpt b (n/2)) * (fastexpt b (n/2))
else b * (fastexpt b (n-1));;