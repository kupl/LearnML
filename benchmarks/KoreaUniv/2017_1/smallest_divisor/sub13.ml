(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec init n i =
if i*i > n then n
else if (n mod i = 0) then i
else init n (i+1) in 
init n 2;;