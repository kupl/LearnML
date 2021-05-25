let rec sigma (a,b,f) = 
	if (b - a) < 0 then 0
	else (f a) + sigma((a+1),b,f)
