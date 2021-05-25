let rec iter : int * ('a->'a) -> ('a->'a) = fun(n,f) ->
(
	if (n<=0) then ( fun(x:'a) -> x )
	else ( fun(x:'a)-> ( iter(n-1,f) (f x) ))
	
)
