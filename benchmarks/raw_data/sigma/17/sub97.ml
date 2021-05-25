let rec sigma ((int1 : int), (int2 : int), (f : int->int)) : int =
	if int1==int2 then f int2
	else if int2>int1 then sigma (int1+1, int2, f) + f int1
	else 0