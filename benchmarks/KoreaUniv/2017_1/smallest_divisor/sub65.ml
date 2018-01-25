(*problem 2*)
let smallest_divisor : int -> int = fun n 
-> let rec sd n i 
= if i*i>n then n 
	else if (n mod i = 0) then i 
		else sd n (i + 1) in sd n 2;;