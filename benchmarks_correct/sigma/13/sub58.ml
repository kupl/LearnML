let rec sigma f sn en =
	if sn>en then 0
    else if sn=en then (f sn)
    else (f en) + (sigma f sn (en-1))