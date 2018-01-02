let rec f x = 
	if x=1 then 0
	else if x=2 then 1
	else f (x-1) + f (x-2);;