let rec iter (n, (f : 'a -> 'a)) : 'a -> 'a =
	if n = 0
			then fun x -> x
			else fun x -> iter (n-1,f) (f x)
