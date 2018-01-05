let rec sigma (n1, n2, f) =
	if (n1>n2) then 0
	else if (n1=n2) then f n1
	else f n1 + sigma ((n1+1), n2, f)