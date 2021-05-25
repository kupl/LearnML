let rec sigma(s,e,f) =
	if s>e then 0
		else if s=e then f s
			else f s + sigma(s+1,e,f)