let rec sigma f x y =
	if x = y then f y
	else sigma f (x+1) y + f x;;
