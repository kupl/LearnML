let rec sigma f n1 n2 =
	if (n1>n2) then 0
	else if (n1=n2) then f n1
	else f n1 + sigma f (n1+1) n2