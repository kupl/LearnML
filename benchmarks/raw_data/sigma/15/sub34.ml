let rec sigma (a, b, funx) =
		if a>b then 0
		else (funx a) + sigma (a+1, b, funx)
