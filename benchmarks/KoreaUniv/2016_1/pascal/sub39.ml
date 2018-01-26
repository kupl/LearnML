let rec pascal (a, b) =
	if b = 0 then 1
	else if b =  a then 1
	else (pascal ((a-1), (b-1))) + (pascal ((a-1), b));;
