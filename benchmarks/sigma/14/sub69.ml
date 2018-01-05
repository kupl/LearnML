let rec sigma (a,b,f)
	= if a > b then 0
	else f(b) + sigma (a,b-1,f);;