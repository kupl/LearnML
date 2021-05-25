let rec iter ((n : int), (f : 'a -> 'a)) (a : 'a) : 'a =
	if n == 0 then a
	else iter (n-1, f) (f a)