let rec iter (n, func) num = 
	if n > 0 then (func (iter ((n-1), func) num))
	else num
	      
