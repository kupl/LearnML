let rec sigma f int1 int2 =
	if int1==int2 then f int2
	else if int2>int1 then sigma f (int1+1) int2 + f int1
	else 0