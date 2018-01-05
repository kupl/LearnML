let rec sigma ((a : int),(b : int),(f : int -> int)) : int =
	if (a<b) then (f a) + (sigma (a+1,b,f))
	else if(a==b) then (f a)
	else 0

