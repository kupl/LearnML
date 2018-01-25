let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) ->
	if n=0 then fun x->x
	else if n>0 then let k = iter(n-1,f) in fun x-> k (f x)
	else raise Invalid
