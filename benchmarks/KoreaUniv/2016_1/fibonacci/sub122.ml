let rec fib (x) =
	if (x = 0) then 0
	else if (x = 1) then 1
	else (fib (x-1)) +  (fib (x-2));;
