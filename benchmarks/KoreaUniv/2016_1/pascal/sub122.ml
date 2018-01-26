let rec pascal (x, y) =
	if y = 0 then 1
	else if x = y then 1
	else if x < y then raise (Failure "x >= y value")
	else pascal (x-1, y) + pascal (x-1, y-1);;
