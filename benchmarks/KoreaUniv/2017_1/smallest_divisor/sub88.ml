(* problem 2*)
let rec inner n i = if i>(n/2) then n else
if (n mod i = 0) then i else inner n (i+1);;
let smallest_divisor : int -> int
= fun n -> inner n 2;;