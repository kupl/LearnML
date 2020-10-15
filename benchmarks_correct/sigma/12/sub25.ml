let rec sigma func int1 int2 =
	if(int1 > int2) then 0
	else if(int1 == int2) then (func int1)
	else (func int1) + sigma func (int1+1) int2
