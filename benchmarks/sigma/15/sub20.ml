let rec sigma (a,b,ft) = 
	if a>b then 0
	else (ft a)+sigma(a+1,b,ft)