(* problem 2*)

let rec find_div : int -> int -> int
= fun n x ->
if n mod x = 0 then x
else if x*x > n then n
else find_div n (x+1)

let smallest_divisor : int -> int
= fun n -> find_div n 2