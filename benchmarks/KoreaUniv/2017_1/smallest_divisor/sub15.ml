(* problem 2*)
let rec divide : int -> int -> int
= fun n i -> if n mod i = 0 then (n/i)
	     else if (i * i) <= n then n
		  else divide n (i-1);;

let smallest_divisor : int -> int
= fun n -> divide n (n-1);;