exception NO_NEGATIVE_INTEGERS
(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> 
if n == 0 then 1
else if n<0 then raise (NO_NEGATIVE_INTEGERS)
else if (n mod 2) == 0 then fastexpt b (n/2) * fastexpt b (n/2)
else b * fastexpt b (n-1)
