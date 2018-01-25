(* problem 2*)
let rec smallest_helper n k = if (n mod k)=0 then k else if float_of_int k>sqrt(float_of_int n) then n else smallest_helper n (k+1);;
let smallest_divisor : int -> int
= fun n -> smallest_helper n 2;;