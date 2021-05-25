let rec iter : int*('a->'a)->'a->'a =function(n,f) ->
	if n=0 then (function x->x)
	else function x -> iter(n-1, f) (f x)
