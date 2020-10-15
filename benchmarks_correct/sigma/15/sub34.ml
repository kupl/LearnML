let rec sigma funx a b  =
		if a>b then 0
		else (funx a) + sigma funx (a+1) b
