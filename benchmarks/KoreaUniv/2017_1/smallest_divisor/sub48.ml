(* problem 2*)

let smallest_divisor : int -> int
= fun n -> 
let a = 2 in
let rec loop a n =
	if ((n mod a)=0) then a
else if ((a*a)>n) then n
else (loop (a+1) n)
in loop a n