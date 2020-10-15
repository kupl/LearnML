let rec sigma fn i1 i2=
	if i1 > i2 then 0
	else fn (i1) + sigma fn (i1+1) i2
