(*problem 2*)
let rec sqrt : int -> int -> int
= fun n a ->
	if((a*a) > n) then n
	else if (n mod a = 0) then a
	else sqrt n (a+1)