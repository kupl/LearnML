let rec sigma : (int * int * (int -> int)) -> int = fun (i1, i2, fn) ->
	if i1 > i2 then 0
	else fn (i1) + sigma (i1+1, i2, fn)
