let rec sigma (a,b,func) =
	if a>b then 0
	else 
		sigma((a+1),b,func)+(func a)	
	
