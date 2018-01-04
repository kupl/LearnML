let rec iter ((n:int), (f)) (id) = 
	if n=0 then id else f(iter(n-1,f)(id))