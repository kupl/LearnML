let rec sigma f s e =
	if s>e then 0
		else if s=e then f s
			else f s + sigma f (s+1) e