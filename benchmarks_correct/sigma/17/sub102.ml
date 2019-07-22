let rec sigma: int * int * (int->int) -> int = fun(a,b,f) ->
(
	if (b < a) then (0)
	else ( f b + sigma(a,b-1,f) )

)
