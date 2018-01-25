(* problem 2*)

let smallest_divisor : int -> int
= fun n -> let rec loop n x = 
	if float_of_int x > sqrt (float_of_int n) then n (*prime number*)
	else if (n mod x =0) then x
	else loop n (x+1)
		in loop n 2 