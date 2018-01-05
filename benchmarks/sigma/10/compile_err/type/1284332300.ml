let rec sigma a b f = 
	if a > b then f a
	else a + sigma (a+1) b f