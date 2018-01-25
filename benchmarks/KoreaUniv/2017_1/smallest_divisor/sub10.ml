(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec loop x = 
if x > int_of_float (sqrt (float_of_int n)) then n
else if n mod x == 0 then x
else (loop (x+1))
in (loop 2)