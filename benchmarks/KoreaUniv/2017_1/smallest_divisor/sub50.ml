(* problem 2*)
let rec divisorble i n =
if n mod i = 0 then i
else if i = int_of_float(sqrt(float_of_int(n))) then n
else divisorble (i+1) n;;

let smallest_divisor : int -> int
= fun n -> divisorble 2 n;;