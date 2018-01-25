(* problem 2*)
let smallest_divisor : int -> int
= fun n -> let rec loop i =
	if i*i > n then n
	else if (n mod i = 0) then i
	else loop (i+1)
	in loop 2