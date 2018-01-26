let rec pascal (a,b) =
	if a = 0 then 1
	else if b = 0 || b = a then 1
	else pascal(a-1,b-1) + pascal(a-1,b)
