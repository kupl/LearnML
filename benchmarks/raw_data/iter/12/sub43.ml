let rec iter (n,f) = 
	match (n,f) with
	|(0,f) -> fun x -> x
	|(n,f) -> fun x -> iter(n-1, f)(f x)
