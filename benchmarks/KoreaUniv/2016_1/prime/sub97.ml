let prime n =
	if n = 0 || n = 1 then false
	else if n = 2 then true
	else let rec isprime a = if a*a > n then true
			else (n mod a) <> 0 && isprime (a+1)
		in isprime 2
		 
