let rec sigma func a b  = 
	if a>b then 0
	else 
		sigma func (a+1) b+(func a)	
	
