let rec iter(n,f) = 
	if n > 0 then fun x -> iter(n-1,f)(f x)
	else fun x -> x
