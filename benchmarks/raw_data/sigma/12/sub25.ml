let rec sigma(int1, int2, func) =
	if(int1 > int2) then 0
	else if(int1 == int2) then (func int1)
	else (func int1) + sigma(int1+1, int2, func)
