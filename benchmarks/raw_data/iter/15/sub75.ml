let rec iter ((n: int), (f: int -> int)) (a: int) :int = 
	if (n = 0) then a
	else (f (iter ((n-1), f) a))