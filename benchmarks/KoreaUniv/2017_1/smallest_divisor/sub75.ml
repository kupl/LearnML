(* problem 2*)
let smallest_divisor : int -> int
= fun n -> let rec div n m i = if m<i then n else if n mod i = 0 then i else (div n m (i+1)) in div n (int_of_float(sqrt(float_of_int n))) 2;;