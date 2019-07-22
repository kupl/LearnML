let rec sigma (a,b,f) = 
	if a > b then raise (Failure "Invalid")
	else if a = b then f a 
	else (f a)+sigma (a+1,b,f)