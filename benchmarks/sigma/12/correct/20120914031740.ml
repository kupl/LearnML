let rec sigma (a, b, f) =
	if a=b then (f b) else (f b)+(sigma (a, (b-1), f))
;;
