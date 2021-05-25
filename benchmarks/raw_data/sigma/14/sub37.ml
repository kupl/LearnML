exception Todo

let rec sigma (a, b, f): int  =
	if a = b then (f b)
	else if a > b then 0
	else (f a) + (sigma ((a+1), b, f))
	
	
