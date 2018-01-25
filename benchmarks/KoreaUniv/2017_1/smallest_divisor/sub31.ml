(* problem 2*)

let rec divisor : int -> int -> int
= fun n i ->
	if n <= 0 then raise (Failure "wrong input n") else
	if i*i > n then n 
	else if (n mod i = 0) then i
	else (divisor n (i+1))

let smallest_divisor : int -> int
= fun n -> divisor n 2 