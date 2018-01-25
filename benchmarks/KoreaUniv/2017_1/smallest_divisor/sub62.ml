(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec aux n i = 
         if i > (n / 2) then n 
         else if (n mod i == 0) then i else aux n (i + 1) in aux n 2;;