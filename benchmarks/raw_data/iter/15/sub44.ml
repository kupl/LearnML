let rec iter : int * ('a -> 'a) -> ('a -> 'a) = fun (n, f) ->
	if n <= 0 then fun x -> x
	else fun (x : 'a) -> f (iter(n-1, f) x)
