let rec sigma : int*int*(int->int)->int
= function(a,b,f) ->
	if a>b then 0
	else (f(a) + sigma(a+1,b,f))
