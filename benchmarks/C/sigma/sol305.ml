let rec sigma ft a b  =
	if a>b then 0
	else (ft a)+sigma ft (a+1) b