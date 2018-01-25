(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec doo n i = 
  if i>n then n else
  if (n mod i = 0) then i 
  else doo n(i+1) in 
  doo n 2;;
